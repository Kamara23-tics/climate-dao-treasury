{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE NumericUnderscores   #-}
-- Added NumericUnderscores for clearer integer values (like 3_000_000)

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T
-- System.IO is needed to prevent encoding errors when printing the address
import System.IO (hSetEncoding, stdout, utf8) 

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo, txInfoValidRange, scriptContextTxInfo, findOwnInput)
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval (contains, from, to)
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless, divide) -- Added 'divide' to exclude from Prelude
import qualified PlutusTx.Builtins as Builtins


-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer for DAO Treasury 🌳
------------------------------------------------------------------------

-- | Stores the state of a single Proposal funding request.
data ProposalDatum = ProposalDatum
    { pdProposer     :: PubKeyHash    -- The public key hash of the entity submitting the proposal.
    , pdFundingGoal  :: Integer       -- The minimum ADA amount (in Lovelace) required for the project.
    , pdVotingDeadline :: POSIXTime     -- The time until which the proposal can be canceled.
    , pdRecipient    :: PubKeyHash    -- The public key hash of the entity receiving the funds (e.g., the solar farm builder).
    }
PlutusTx.unstableMakeIsData ''ProposalDatum

-- | Actions that can be performed on the Treasury UTXO.
data TreasuryAction
  = FundProposal PubKeyHash
  | CancelProposal

PlutusTx.unstableMakeIsData ''TreasuryAction

------------------------------------------------------------------------
-- Helpers (Minimal set for this contract)
------------------------------------------------------------------------

{-# INLINABLE scriptInputContainsAda #-}
-- Checks that the script UTXO contains at least the required funding goal in ADA.
scriptInputContainsAda :: ScriptContext -> Integer -> Bool
scriptInputContainsAda ctx requiredAmount =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v adaSymbol adaToken >= requiredAmount

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------
-- [Image of a smart contract flow diagram for a DAO Funding Proposal showing two paths: 
-- one path for 'FundProposal' requiring Proposer signature AND time passed deadline, 
-- and a second path for 'CancelProposal' requiring Proposer signature AND time before deadline]

{-# INLINABLE mkValidator #-}
mkValidator :: ProposalDatum -> TreasuryAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      FundProposal ->
           -- A. Check if the transaction is signed by the proposer
           traceIfFalse "proposer signature missing" (txSignedBy info (pdProposer dat)) &&
           -- B. Check that the deadline has passed (implies successful voting period)
           traceIfFalse "voting period not over"   (afterDeadline) &&
           -- C. Check that the required ADA is present in the input
           traceIfFalse "insufficient funds in script" (scriptInputContainsAda ctx (pdFundingGoal dat)) &&
           -- D. Check that the funds are paid to the recipient
           traceIfFalse "recipient not paid"         recipientPaid
      
      CancelProposal ->
           -- A. Check if the transaction is signed by the proposer
           traceIfFalse "proposer signature missing" (txSignedBy info (pdProposer dat)) &&
           -- B. Check that the deadline has NOT passed (allows cancellation early)
           traceIfFalse "cancellation period elapsed" beforeDeadline &&
           -- C. Check that the funds are returned to the proposer
           traceIfFalse "funds not returned to proposer" proposerRefunded
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    -- Funding is allowed only AFTER the voting deadline
    afterDeadline :: Bool
    afterDeadline = Interval.contains (Interval.from (pdVotingDeadline dat + 1)) txRange

    -- Cancellation is allowed only BEFORE the voting deadline
    beforeDeadline :: Bool
    beforeDeadline = Interval.contains (Interval.to (pdVotingDeadline dat)) txRange

    -- Check: Recipient receives at least the funding goal in ADA
    recipientPaid :: Bool
    recipientPaid =
      let v = valuePaidTo info (pdRecipient dat)
      in valueOf v adaSymbol adaToken >= pdFundingGoal dat

    -- Check: Proposer receives at least the funding goal in ADA (for refund)
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
-- Validator Hash + Addresses (Unchanged)
------------------------------------------------------------------------

-- Compute validator hash using only plutus-ledger-api + plutus-tx
plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

-- Derive the Plutus script address from the hash
plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing


-- Off-chain (Cardano API) Bech32 address for CLI use
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
-- File writing (Unchanged)
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main (Updated output text)
------------------------------------------------------------------------

main :: IO ()
main = do
    hSetEncoding stdout utf8
    
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "climate-dao-treasury.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator


    putStrLn "\n--- Climate DAO Treasury Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "-----------------------------------------"

    putStrLn "Climate DAO Treasury validator generated successfully."