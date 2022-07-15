import React from "react";
import { IpcChannel } from "~/base-components/ipc";
import redux from "~/base-components/redux";

class KeypairManager {
  constructor() {
    this.channel = new IpcChannel("keypair");
    this.onKeypairUpdated = null;
    this.eventTarget = new EventTarget();
    this._kp = null;

    this.keypairNames = {};

    this.signReqModal = React.createRef();
    this.channel.on("signTransaction", this.signTransaction.bind(this));
  }

  set kp(kp) {
    this._kp = kp;
  }

  get kp() {
    return this._kp;
  }

  onUpdated(callback) {
    const eventHandler = event => callback(event.detail);
    this.eventTarget.addEventListener("updated", eventHandler);
    return () => this.eventTarget.removeEventListener("updated", eventHandler);
  }

  getKeypairFromRedux(networkId) {
    const keypairsState = redux.getState().keypairs;
    const formatjs = Object.values(keypairsState.toJS());
    const unsorted = formatjs.map(keypair => ({
      address: keypair.address,
      name: keypair.name,
      balance: (keypair.balance && keypair.balance[networkId]) || "0",
    }));
    return unsorted.sort((a, b) => {
      if (!a.name || !b.name) {
        return 0;
      }
      return a.name.localeCompare(b.name);
    });
  }

  async loadAllKeypairs() {
    try {
      const { networkManager } = require("~/ligo-components/eth-network");
      const networkId = networkManager?.network?.id;
      let keypairs = await this.channel.invoke("get");
      keypairs = keypairs.filter(item => item.address !== "");
      redux.dispatch("UPDATE_FROM_REMOTE", keypairs);

      keypairs.forEach(
        item =>
          (item.address = networkManager?.sdk?.utils?.simplifyAddress(item.address) || item.address)
      );
      const sorted = this.getKeypairFromRedux(networkId);

      const updating = sorted.map(async keypair => {
        if (networkId === void 0) return;
        const { address } = keypair;
        const account = await (networkManager?.sdk?.client?.getAccount(address) || {
          balance: "0.0",
        });
        redux.dispatch("UPDATE_KEYPAIR_BALANCE", {
          address,
          networkId,
          balance: account.balance,
        });
      });
      Promise.all(updating).then(() => {
        const keypairs = this.getKeypairFromRedux(networkId);
        const event = new CustomEvent("updated", { detail: keypairs });
        this.eventTarget.dispatchEvent(event);
      });
      return sorted;
    } catch (e) {
      // notification.error('Error', e.message)
      return [];
    }
  }

  async newKeypair(kp, chain, secretType) {
    return kp.newKeypair(chain, secretType);
  }

  async loadAndUpdateKeypairs() {
    const keypairs = await this.loadAllKeypairs();

    keypairs.forEach(k => (this.keypairNames[k.address] = k.name));

    const event = new CustomEvent("updated", { detail: keypairs });
    this.eventTarget.dispatchEvent(event);
  }

  async saveKeypair(name, keypair) {
    keypair.name = name;
    await this.channel.invoke("post", "", keypair);
    redux.dispatch("UPDATE_KEYPAIR", { address: keypair.address, name });
    await this.loadAndUpdateKeypairs();
  }

  async updateKeypairName(address, name) {
    redux.dispatch("UPDATE_KEYPAIR", { address, name });
    await this.channel.invoke("put", address, { name });
    await this.loadAndUpdateKeypairs();
  }

  async deleteKeypair(keypair) {
    await this.channel.invoke("delete", keypair.address);
    redux.dispatch("REMOVE_KEYPAIR", { address: keypair.address });
    await this.loadAndUpdateKeypairs();
  }

  getName(address) {
    return this.keypairNames[address];
  }

  async getKeypair(address) {
    const keypair = await this.channel.invoke("get", address);
    if (!keypair) {
      throw new Error(`No keypair for <b>${address}</b>`);
    }
    return keypair;
  }

  async getSecret(address, key = "secret") {
    const keypair = await this.getKeypair(address);
    if (!keypair[key]) {
      throw new Error(`No ${key} for <b>${address}</b>`);
    }
    return keypair[key];
  }

  async signTransaction(id, tx) {
    try {
      const modified = await this.signReqModal.current?.openModal(tx);
      this.channel.invoke("callback", id, null, modified);
    } catch {
      this.channel.invoke("callback", id, "User rejected the transaction.");
    }
  }
}

export default new KeypairManager();
