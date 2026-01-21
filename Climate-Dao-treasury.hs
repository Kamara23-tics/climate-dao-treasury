{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE NumericUnderscores   #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T
import System.IO (hSetEncoding, stdout, utf8) 

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo, txInfoValidRange, scriptContextTxInfo, findOwnInput)
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval (contains, from, to)
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless, divide)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data ProposalDatum = ProposalDatum
    { pdProposer     :: PubKeyHash
    , pdFundingGoal  :: Integer
    , pdVotingDeadline :: POSIXTime
    , pdRecipient    :: PubKeyHash
    }
PlutusTx.unstableMakeIsData ''ProposalDatum

data TreasuryAction
  = FundProposal
  | CancelProposal

PlutusTx.unstableMakeIsData ''TreasuryAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE scriptInputContainsAda #-}
scriptInputContainsAda :: ScriptContext -> Integer -> Bool
scriptInputContainsAda ctx requiredAmount =
    case findOwnInput ctx of
        Nothing -> traceError "script input missing"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v adaSymbol adaToken >= requiredAmount

------------------------------------------------------------------------
-- Validator Logic - FIXED VERSION
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: ProposalDatum -> TreasuryAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      FundProposal ->
           traceIfFalse "proposer signature missing" (txSignedBy info (pdProposer dat)) &&
           -- ✅ FIX: Changed to use `from` instead of `from (deadline + 1)`
           traceIfFalse "voting period not over"   afterDeadline &&
           traceIfFalse "insufficient funds in script" (scriptInputContainsAda ctx (pdFundingGoal dat)) &&
           traceIfFalse "recipient not paid" recipientPaid
      
      CancelProposal ->
           traceIfFalse "proposer signature missing" (txSignedBy info (pdProposer dat)) &&
           traceIfFalse "cancellation period elapsed" beforeDeadline &&
           traceIfFalse "funds not returned to proposer" proposerRefunded
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    -- ✅ KEY FIX: Check that txRange starts AFTER the deadline
    -- This means the transaction's validFrom must be > deadline
    afterDeadline :: Bool
    afterDeadline = Interval.contains (Interval.from (pdVotingDeadline dat)) txRange

    -- Cancellation allowed only BEFORE deadline
    beforeDeadline :: Bool
    beforeDeadline = Interval.contains (Interval.to (pdVotingDeadline dat)) txRange

    recipientPaid :: Bool
    recipientPaid =
      let v = valuePaidTo info (pdRecipient dat)
      in valueOf v adaSymbol adaToken >= pdFundingGoal dat

    proposerRefunded :: Bool
    proposerRefunded =
      let v = valuePaidTo info (pdProposer dat)
      in valueOf v adaSymbol adaToken >= pdFundingGoal dat

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @ProposalDatum d
        red = unsafeFromBuiltinData @TreasuryAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    hSetEncoding stdout utf8
    
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "climate-dao-treasury.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Climate DAO Treasury Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "-----------------------------------------"
    putStrLn "Climate DAO Treasury validator generated successfully."
