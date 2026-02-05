import {
  Lucid,
  Blockfrost,
  Constr,
  Data,
} from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";

let lucid;
let walletAddress;
let scriptAddress;

/* ===============================
   Validator Hash - UPDATED WITH CANCELLATION FEE
   =============================== */
const CLIMATE_DAO_VALIDATOR = "590fbf010000323232323233223233322232323232323232332232323322323232323232323232332232323233322232323232323232323232323232232323232232232325335323232325335003153355335330105001350042222004103f13357389211a70726f706f736572207369676e6174757265206d697373696e670003e1533553353335014501b335015335016503b03f3350183503c35004222200203f355001222222222222005103f133573892011b63616e63656c6c6174696f6e20706572696f6420656c61707365640003e15335333573466e20ccc03cccd54c08448005407d40b8cc04cd401088880114004048048d4c8c8cd540c4cdc08012800a800899b8333704002900a24190026a00844440064400407c07e207e266ae7124012c70726f706f73657220726566756e6420696e636f72726563742028666565206e6f74206465647563746564290003e103e103e153355335330105001350042222004103f133573892011a70726f706f736572207369676e6174757265206d697373696e670003e1533553353335014501b3350153350163503c35004222200203f335018503a03f355001222222222222005103f1335738920116766f74696e6720706572696f64206e6f74206f7665720003e1533553353253353500322350022222222222223333500d25041250412504123335530351200150212350012253355335333573466e3cd400888008d4010880081481444ccd5cd19b873500222001350042200105205110511350450031504400d21333573466e20ccc044d4d400488004888800c0500500081001044c98c8108cd5ce2491473637269707420696e707574206d697373696e6700042350042222003103f133573892011c696e73756666696369656e742066756e647320696e207363726970740003e15335333573466e20ccc03cccd54c08448005407d40b8cc04cd401088880054004048048d4010888800c0f80fc40fc4cd5ce24912726563697069656e74206e6f7420706169640003e103e103e103e135001220023333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd40d40d8d5d0a80619a81a81b1aba1500b33503503735742a014666aa072eb940e0d5d0a804999aa81cbae503835742a01066a06a0866ae85401cccd540e4111d69aba150063232323333573466e1cd55cea80124000466a04c6464646666ae68cdc39aab9d5002480008cd40accd4139d69aba150023052357426ae8940088c98c8158cd5ce02b82b02a09aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a81699a8273ad35742a00460a46ae84d5d1280111931902b19ab9c057056054135573ca00226ea8004d5d09aba250022326320523357380a60a40a026aae7940044dd50009aba1500533503575c6ae854010ccd540e41008004d5d0a801999aa81cbae200135742a00460846ae84d5d1280111931902719ab9c04f04e04c135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860646ae84d5d1280211931902019ab9c04104003e3333573466e1d40152002212200223333573466e1d40192000212200123263204033573808208007c07a6666ae68cdc39aab9d5009480008cccc888848cccc00401401000c008dd71aba15009375a6ae854020dd69aba15007375c6ae84d5d1280391931901f19ab9c03f03e03c103d16135573ca00226ea80044d55ce9baa001135744a00226ae8940044d55cf280089baa0012223232300100532001355039223350014800088d4008894cd4ccd5cd19b8f00200903a039130070011300600332001355038223350014800088d4008894cd4ccd5cd19b8f0020070390381001130060032235002222222222222533533355302312001500f25335333573466e3c0380040f80f44d40c4004540c0010840f840f04cd4054894cd40088400c4005408122100223500122222222222233355301c1200122350022222350042233500225335333573466e3c05c00410c1084cd40c40180204020802140a40284888d400888d400888d401488d4008894ccd4ccd404002c01800854cd400454cd40144ccd403802c00c01c40d44ccd403802c00c01c40d44ccd403802c00c01c48848cc00400c00848848cc00400c0084888d400888d400c894ccd4ccd402401c01000854cd400c400440bc40b840bc48848cc00400c0084888c8c8c8c94ccd4018854ccd4018854ccd402084c011261300349854ccd401c84c01126130034984038403054ccd401c84c011261300349854ccd401884c0112613003498403454ccd40148402c4030402854ccd4014854ccd401c84c015261300449854ccd401884c01526130044984034402c54ccd401884c015261300449854ccd401484c0152613004498403094ccd4014854ccd401c854ccd401c84ccd402c028008004585858403054ccd4018854ccd401884ccd4028024008004585858402c402894ccd4010854ccd4018854ccd401884ccd4028024008004585858402c54ccd4014854ccd401484ccd40240200080045858584028402494ccd400c854ccd4014854ccd401484ccd4024020008004585858402854ccd4010854ccd401084ccd402001c0080045858584024402094ccd4008854ccd4010854ccd401084ccd402001c008004585858402454ccd400c854ccd400c84ccd401c0180080045858584020401c48d40048888888801c448cccccccc00488ccd5cd19b87002001027026225335333573466e1c00800409c098401854cd4ccd5cd19b890020010270261004100522333573466e2000800409c09888ccd5cd19b8900200102702622333573466e2400800409809c88ccd5cd19b88002001026027225335333573466e2400800409c09840044008894cd4ccd5cd19b890020010270261002100112220031222002122200112233553007120012350012233550150023355300a12001235001223355018002333500123302d4800000488cc0b80080048cc0b400520000013355300712001235001223355015002333500123355300b1200123500122335501900235500d0010012233355500800f00200123355300b1200123500122335501900235500c00100133355500300a002001111222333553004120015010335530071200123500122335501500235500900133355300412001223500222533533355300c120013233500e223335003220020020013500122001123300122533500210291001026235001223300a00200500610031335014004003501100133553007120012350012232335501600330010053200135502a225335001135500a003221350022253353300c002008112223300200a0041300600300232001355023221122253350011002221330050023335530071200100500400111212223003004112122230010043200135502022112253350011500e22133500f300400233553006120010040013200135501f2211222533500113500322001221333500522002300400233355300712001005004001122123300100300222333573466e3c00800406806448c88c008dd6000990009aa80e911999aab9f0012500a233500930043574200460066ae880080708c8c8cccd5cd19b8735573aa004900011991091980080180118079aba150023005357426ae8940088c98c8070cd5ce00e80e00d09aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180c1aba15002335010017357426ae8940088c98c8084cd5ce01101080f89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6404666ae7009008c08408007c4d55cea80089baa00135742a00466a018eb8d5d09aba2500223263201d33573803c03a03626ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab0013200135501a223233335573e0044a010466a00e66aa012600c6aae754008c014d55cf280118021aba200301a1357420022244004244244660020080062244246600200600424464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900b99ab9c018017015014135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900a99ab9c016015013012011010135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900899ab9c01201100f135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c803ccd5ce00800780689baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c8060cd5ce00c80c00b00a80a00980900880809aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6402266ae7004804403c0384d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200e33573801e01c01801626aae7540044dd500089119191999ab9a3370ea00290021280311999ab9a3370ea004900111a80498031aba135573ca00846666ae68cdc3a801a40004a012464c6401e66ae7004003c03403002c4d55cea80089baa001121222300300411222002112220012323333573466e1d40052002200523333573466e1d40092000200523263200833573801201000c00a26aae74dd5000891001091000a4c240029210350543100223370000400222464600200244660066004004003";

const script = {
  type: "PlutusV2",
  script: CLIMATE_DAO_VALIDATOR,
};

/* ===============================
   Cancellation Fee Constants
   =============================== */
const CANCELLATION_FEE_NUMERATOR = 10;
const CANCELLATION_FEE_DENOMINATOR = 100;

function calculateRefundAmount(fundingGoalLovelace) {
  const fee = (fundingGoalLovelace * BigInt(CANCELLATION_FEE_NUMERATOR)) / BigInt(CANCELLATION_FEE_DENOMINATOR);
  const refund = fundingGoalLovelace - fee;
  return { refund, fee };
}

/* ===============================
   INIT LUCID
   =============================== */
export async function initLucid() {
  try {
    lucid = await Lucid.new(
      new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        "preprodYjRkHfcazNkL0xxG9C2RdUbUoTrG7wip"
      ),
      "Preprod"
    );

    const api = await window.cardano.lace.enable();
    lucid.selectWallet(api);

    walletAddress = await lucid.wallet.address();
    scriptAddress = lucid.utils.validatorToAddress(script);

    console.log("Connected wallet:", walletAddress);
    console.log("Script address:", scriptAddress);

    const walletInfoEl = document.getElementById("walletInfo");
    const walletAddressEl = document.getElementById("walletAddress");
    if (walletInfoEl && walletAddressEl) {
      walletAddressEl.textContent = walletAddress;
      walletInfoEl.classList.remove("hidden");
    }

    const recipientInput = document.getElementById("recipient");
    if (recipientInput && !recipientInput.value) {
      recipientInput.value = walletAddress;
    }

    document.getElementById("app").classList.remove("hidden");
    log("✅ Wallet connected successfully!");
    log(`📍 Script address: ${scriptAddress}`);
    log(`⚠️ NOTE: Cancellation fee is ${CANCELLATION_FEE_NUMERATOR}% of funding goal`);
  } catch (error) {
    console.error("❌ Wallet connection error:", error);
    log(`❌ Error: ${error.message}`);
    throw error;
  }
}

/* ===============================
   DATUM & REDEEMERS
   =============================== */
function mkProposalDatum(proposerPkh, fundingGoal, votingDeadline, recipientPkh) {
  return Data.to(
    new Constr(0, [
      proposerPkh,
      BigInt(fundingGoal),
      BigInt(votingDeadline),
      recipientPkh,
    ])
  );
}

// FundProposal is Constr(0) - releases funds to RECIPIENT after deadline
function redeemerFundProposal() {
  return Data.to(new Constr(0, []));
}

// CancelProposal is Constr(1) - refunds to PROPOSER before deadline (with 10% fee)
function redeemerCancelProposal() {
  return Data.to(new Constr(1, []));
}

/* ===============================
   ADDRESS VALIDATION
   =============================== */
function validateAndExtractPkh(address) {
  try {
    const addressDetails = lucid.utils.getAddressDetails(address);
    
    if (!addressDetails.paymentCredential) {
      throw new Error("Invalid address: No payment credential found");
    }
    
    return addressDetails.paymentCredential.hash;
  } catch (error) {
    console.error("Address validation error:", error);
    throw new Error(`Invalid Cardano address: ${error.message}`);
  }
}

/* ===============================
   CREATE PROPOSAL
   =============================== */
export async function createProposal() {
  try {
    const fundingGoalAda = document.getElementById("fundingGoal").value;
    const votingDeadline = document.getElementById("votingDeadline").value;
    const recipientAddr = document.getElementById("recipient").value;

    log("📝 Validating inputs...");

    if (!fundingGoalAda || parseFloat(fundingGoalAda) < 1) {
      throw new Error("Funding goal must be at least 1 ADA");
    }
    
    if (!votingDeadline || votingDeadline.trim() === "") {
      throw new Error("Voting deadline is required");
    }
    
    if (!recipientAddr || recipientAddr.trim() === "") {
      throw new Error("Recipient address is required");
    }

    const fundingGoal = BigInt(Math.floor(parseFloat(fundingGoalAda) * 1_000_000));
    const deadline = BigInt(Number(votingDeadline));

    const proposerPkh = lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;
    const recipientPkh = validateAndExtractPkh(recipientAddr);

    log(`📋 Creating proposal: ${fundingGoalAda} ADA until ${new Date(Number(deadline)).toLocaleString()}`);
    log(`⚠️ If you cancel, you'll receive ${100 - CANCELLATION_FEE_NUMERATOR}% back (${CANCELLATION_FEE_NUMERATOR}% fee)`);

    const datum = mkProposalDatum(proposerPkh, fundingGoal, deadline, recipientPkh);

    log("🔨 Building transaction...");

    const tx = await lucid
      .newTx()
      .payToContract(scriptAddress, { inline: datum }, { lovelace: fundingGoal })
      .addSignerKey(proposerPkh)
      .complete();

    log("✍️ Signing transaction...");
    const signed = await tx.sign().complete();
    
    log("📤 Submitting to blockchain...");
    const txHash = await signed.submit();

    console.log("✅ Proposal created TX:", txHash);
    log(`✅ SUCCESS! Proposal created!`);
    log(`TX: ${txHash}`);
    log(`Explorer: https://preprod.cardanoscan.io/transaction/${txHash}`);
    
    if (window.setProposalDeadline) {
      window.setProposalDeadline(votingDeadline);
    }
    
    return txHash;
  } catch (error) {
    console.error("❌ Error creating proposal:", error);
    log(`❌ ERROR: ${error.message}`);
    throw error;
  }
}

/* ===============================
   CANCEL PROPOSAL - WITH FEE DEDUCTION
   =============================== */
export async function cancelProposal() {
  try {
    const proposerPkh =
      lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

    log("🔍 Searching for YOUR proposals...");

    const utxos = await lucid.utxosAt(scriptAddress);

    let myProposals = [];

    for (const utxo of utxos) {
      try {
        const datumData = Data.from(utxo.datum);
        const datumProposer = datumData.fields[0];

        if (datumProposer === proposerPkh) {
          myProposals.push({ utxo, datumData });
        }
      } catch (_) {}
    }

    if (myProposals.length === 0) {
      throw new Error("No proposals created by your wallet.");
    }

    const now = Date.now();

    const activeProposals = myProposals.filter(p =>
      now < Number(p.datumData.fields[2])
    );

    if (activeProposals.length === 0) {
      throw new Error("All proposals have expired. Use Release Funds instead.");
    }

    activeProposals.sort(
      (a, b) =>
        Number(b.datumData.fields[2]) - Number(a.datumData.fields[2])
    );

    const { utxo: targetUtxo, datumData } = activeProposals[0];
    const deadline = Number(datumData.fields[2]);
    const fundingGoal = targetUtxo.assets.lovelace;

    // Calculate refund with fee deduction
    const { refund, fee } = calculateRefundAmount(fundingGoal);
    
    const refundAda = (Number(refund) / 1_000_000).toFixed(2);
    const feeAda = (Number(fee) / 1_000_000).toFixed(2);

    log(`⏰ Deadline: ${new Date(deadline).toLocaleString()} (${Math.floor((deadline - now) / 1000)}s remaining)`);
    log(`💰 Original amount: ${(Number(fundingGoal) / 1_000_000).toFixed(2)} ADA`);
    log(`⚠️ Cancellation fee (${CANCELLATION_FEE_NUMERATOR}%): ${feeAda} ADA`);
    log(`💵 You will receive: ${refundAda} ADA`);
    log("🔨 Building cancellation transaction...");

    const tx = await lucid
      .newTx()
      .collectFrom([targetUtxo], redeemerCancelProposal())
      .payToAddress(walletAddress, { lovelace: refund })
      .attachSpendingValidator(script)
      .addSignerKey(proposerPkh)
      .validFrom(now)
      .validTo(deadline - 1)
      .complete();

    log("✍️ Signing transaction...");
    const signed = await tx.sign().complete();
    
    log("📤 Submitting to blockchain...");
    const txHash = await signed.submit();

    log(`✅ Proposal cancelled!`);
    log(`💵 Refund: ${refundAda} ADA (after ${feeAda} ADA fee)`);
    log(`TX: ${txHash}`);
    log(`Explorer: https://preprod.cardanoscan.io/transaction/${txHash}`);

    return txHash;
  } catch (error) {
    log(`❌ ERROR: ${error.message}`);
    throw error;
  }
}

/* ===============================
   RELEASE FUNDS (Send to Recipient)
   =============================== */
export async function releaseFunds() {
  try {
    const proposerPkh =
      lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

    log("🔍 Searching for YOUR proposals...");

    const utxos = await lucid.utxosAt(scriptAddress);

    let myProposals = [];

    for (const utxo of utxos) {
      try {
        const datumData = Data.from(utxo.datum);
        const datumProposer = datumData.fields[0];

        if (datumProposer === proposerPkh) {
          myProposals.push({ utxo, datumData });
        }
      } catch (_) {}
    }

    if (myProposals.length === 0) {
      throw new Error("No proposals found from your wallet.");
    }

    const now = Date.now();
    log(`⏰ Current time: ${new Date(now).toLocaleString()} (${now}ms)`);

    // Find proposals where deadline has passed
    const expiredProposals = myProposals.filter(p => {
      const proposalDeadline = Number(p.datumData.fields[2]);
      return now > proposalDeadline;
    });

    if (expiredProposals.length === 0) {
      const allDeadlines = myProposals.map(p => Number(p.datumData.fields[2]));
      const nextDeadline = Math.min(...allDeadlines);
      const waitSeconds = Math.ceil((nextDeadline - now) / 1000);
      
      throw new Error(
        `No expired proposals yet.\n` +
        `Next deadline: ${new Date(nextDeadline).toLocaleString()}\n` +
        `Wait ${waitSeconds} more seconds before releasing.`
      );
    }

    expiredProposals.sort(
      (a, b) => Number(b.datumData.fields[2]) - Number(a.datumData.fields[2])
    );

    const { utxo: targetUtxo, datumData } = expiredProposals[0];
    const recipientPkh = datumData.fields[3];
    const deadlineNum = Number(datumData.fields[2]);

    const recipientAddress = lucid.utils.credentialToAddress({
      type: "Key",
      hash: recipientPkh,
    });

    log(`📅 Releasing expired proposal (deadline was: ${new Date(deadlineNum).toLocaleString()})`);
    log(`💰 Amount: ${(Number(targetUtxo.assets.lovelace) / 1_000_000).toFixed(2)} ADA`);
    log(`📬 Recipient: ${recipientAddress}`);
    
    // Wait strategy: Make sure we're WELL past the deadline
    const safetyBuffer = 60000; // 1 minute buffer
    const validFromTime = deadlineNum + safetyBuffer;
    
    if (now < validFromTime) {
      const waitMs = validFromTime - now;
      throw new Error(
        `Please wait ${Math.ceil(waitMs / 1000)} more seconds for safety buffer.`
      );
    }
    
    const validToTime = now + 300000; // 5 minutes from now
    
    log(`⏰ Validity window: ${new Date(validFromTime).toLocaleString()} to ${new Date(validToTime).toLocaleString()}`);
    log("🔨 Building release transaction...");

    const tx = await lucid
      .newTx()
      .collectFrom([targetUtxo], redeemerFundProposal())
      .payToAddress(recipientAddress, {
        lovelace: targetUtxo.assets.lovelace,
      })
      .attachSpendingValidator(script)
      .addSignerKey(proposerPkh)
      .validFrom(validFromTime)
      .validTo(validToTime)
      .complete();

    log("✍️ Signing transaction...");
    const signed = await tx.sign().complete();
    
    log("📤 Submitting to blockchain...");
    const txHash = await signed.submit();

    log(`✅ SUCCESS! Funds released to recipient!`);
    log(`TX: ${txHash}`);
    log(`Explorer: https://preprod.cardanoscan.io/transaction/${txHash}`);

    return txHash;
  } catch (error) {
    log(`❌ ERROR: ${error.message}`);
    console.error("Full error:", error);
    throw error;
  }
}

/* ===============================
   LOGGING
   =============================== */
function log(msg) {
  const logEl = document.getElementById("log");
  if (logEl) {
    const timestamp = new Date().toLocaleTimeString();
    logEl.textContent = `[${timestamp}] ${msg}\n${logEl.textContent}`;
  }
  console.log(msg);
}

window.log = log;

/* ===============================
   EVENT HANDLERS
   =============================== */
document.getElementById("connectWallet")?.addEventListener("click", initLucid);
document.getElementById("createBtn")?.addEventListener("click", createProposal);
document.getElementById("cancelBtn")?.addEventListener("click", cancelProposal);
document.getElementById("releaseBtn")?.addEventListener("click", releaseFunds);
