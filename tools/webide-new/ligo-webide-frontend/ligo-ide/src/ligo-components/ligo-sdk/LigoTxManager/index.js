import { importKey } from "@taquito/signer";
import keypairManager from "~/base-components/keypair";
import kp from "../kp";

export default class LigoTxManager {
  constructor(client) {
    this.client = client;
  }

  get provider() {
    return this.client.provider;
  }

  async estimateContract(isWallet, selectedSigner, tzfile, storage) {
    if (isWallet) {
      return await this.provider.estimate.originate({
        code: tzfile,
        init: storage,
      });
    }
    const secret = await keypairManager.getSecret(selectedSigner);
    const secretKey = secret.includes(" ") ? kp.secretFromMnemonic(secret) : secret;
    await importKey(this.provider, secretKey);
    return await this.provider.estimate.originate({
      code: tzfile,
      init: storage,
    });
  }

  async originate(
    tzfile,
    storage,
    selectedSigner,
    isWallet,
    delegateAddress,
    balance,
    gasLimit,
    storageLimit,
    suggestedFeeMutez
  ) {
    if (isWallet) {
      return await this.provider.wallet
        .originate({
          code: tzfile,
          init: storage,
          balance: balance || undefined,
          delegate: delegateAddress || undefined,
          fee: suggestedFeeMutez || undefined,
          gasLimit: gasLimit || undefined,
          storageLimit: storageLimit || undefined,
        })
        .send()
        .then((originationOp) => originationOp.contract())
        .then((contract) => contract.address);
    }
    const secret = await keypairManager.getSecret(selectedSigner);
    const secretKey = secret.includes(" ") ? kp.secretFromMnemonic(secret) : secret;
    await importKey(this.provider, secretKey);
    return await this.provider.contract
      .originate({
        code: tzfile,
        init: storage,
        balance: balance || undefined,
        delegate: delegateAddress || undefined,
        fee: suggestedFeeMutez || undefined,
        gasLimit: gasLimit || undefined,
        storageLimit: storageLimit || undefined,
      })
      .then((originationOp) => originationOp.contract())
      .then((contract) => contract.address);
  }
}
