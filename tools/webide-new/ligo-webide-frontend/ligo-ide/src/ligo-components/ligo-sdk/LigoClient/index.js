import { TezosToolkit } from "@taquito/taquito";
import redux from "~/base-components/redux";

export default class LigoClient {
  constructor(option, browserExtension) {
    const { networkId = "", chainId, url } = option;
    this.networkId = networkId;
    this.chainId = chainId;

    if (browserExtension) {
      this.provider = new TezosToolkit(url);
      this.provider.setWalletProvider(browserExtension.tezos);
    } else {
      throw Error("No chain info");
    }
  }

  get url() {
    return this.provider && this.provider.rpc && this.provider.rpc.url;
  }

  async networkInfo() {
    const chainId = await this.provider.rpc.getChainId();
    return { chainId };
  }

  async getStatus() {
    return this.provider.rpc.getBlock();
  }

  async getAccount(address) {
    const balance = await this.provider.rpc.getBalance(address);
    const regtry = this.provider.format("mutez", "tz", balance).toString();
    return {
      address,
      balance: regtry,
    };
  }
}
