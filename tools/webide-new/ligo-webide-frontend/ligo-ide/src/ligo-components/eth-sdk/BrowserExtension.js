import redux from "~/base-components/redux";
import networks from "./networks";

export default class BrowserExtension {
  constructor(beacon) {
    this.name = "Beacon";
    this._accounts = [];
    this._enabled = false;
    this._enabled = true;
    this.tezos = beacon;
    this.initialize(beacon);
  }

  get isEnabled() {
    return this._enabled;
  }

  get currentAccount() {
    return this.tezos.getPKH();
  }

  get allAccounts() {
    return this._accounts;
  }

  async getChain() {
    return await this.tezos.client.getActiveAccount().then((acc) => {
      if (!acc) {
        return undefined;
      }
      return acc.network;
    });
  }

  async initialize(beacon) {}
}
