import notification from "~/base-components/notification";

export default function makeSdk({ kp, networks, customNetworks = [], Client, TxManager }) {
  let browserExtension;

  return class Sdk {
    static get kp() {
      return kp;
    }

    static get networks() {
      return networks;
    }

    static get customNetworks() {
      return customNetworks;
    }

    constructor({ id, ...option }, browserExtension) {
      this.client = new Client({ networkId: id, ...option }, browserExtension);
      this.networkId = id;
      this.txManager = new TxManager(this.client);
    }

    dispose() {}

    get url() {
      return this.client.url;
    }

    get chainId() {
      return this.client.chainId;
    }

    async networkInfo() {
      return await this.client.networkInfo();
    }

    async getStatus() {
      return await this.client.getStatus();
    }

    async deployContract({
      type,
      tzfile,
      storage,
      selectedSigner,
      isWallet,
      delegateAddress,
      balance,
      gasLimit,
      storageLimit,
      suggestedFeeMutez,
    }) {
      if (type === "origination") {
        return await this.txManager.originate(
          tzfile,
          storage,
          selectedSigner,
          isWallet,
          delegateAddress,
          balance,
          gasLimit,
          storageLimit,
          suggestedFeeMutez
        );
      }
    }

    async estimate({ type, isWallet, selectedSigner, tzfile, storage }) {
      if (type === "origination") {
        return await this.txManager.estimateContract(isWallet, selectedSigner, tzfile, storage);
      }
    }
  };
}
