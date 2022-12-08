import React from "react";
// import { HttpIpcChannel } from "~/base-components/ipc";
import redux from "~/base-components/redux";
import notification from "~/base-components/notification";

type Keypair = {
  address: string;
  secret: string;
  secretName: string;
};

type KpType = {
  newKeypair(secretType: "privkey" | "mnemonic"): Promise<Keypair>;
  importKeypair(secretParam: string): Promise<Keypair>;
  walletFrom(): void;
};

// TODO: use it in Redux
type ReduxKeypair = {
  address: string;
  name: string;
  secret: string;
  balance: { [nId: string]: string };
};

export class KeypairManager {
  // channel: HttpIpcChannel;

  eventTarget: EventTarget;

  _kp: KpType | null;

  keypairNames: { [address: string]: string };

  signReqModal: React.RefObject<unknown>;

  constructor() {
    // this.channel = new HttpIpcChannel("keypair");
    this.eventTarget = new EventTarget();
    // eslint-disable-next-line no-underscore-dangle
    this._kp = null;

    this.keypairNames = {};

    this.signReqModal = React.createRef();
    // this.channel.on("signTransaction", this.signTransaction.bind(this));
  }

  set kp(kp) {
    // eslint-disable-next-line no-underscore-dangle
    this._kp = kp;
  }

  get kp() {
    // eslint-disable-next-line no-underscore-dangle
    return this._kp;
  }

  onUpdated(callback: any) {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-return, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    const eventHandler = (event: any) => callback(event.detail);
    this.eventTarget.addEventListener("updated", eventHandler);
    return () => this.eventTarget.removeEventListener("updated", eventHandler);
  }

  static getKeypairFromRedux(networkId: string) {
    // TODO: use it in Redux

    /* eslint-disable */
    const keypairsState = redux.getState().keypairs;
    const formatjs: ReduxKeypair[] = Object.values(keypairsState.toJS());
    /* eslint-enable */
    let unsorted = formatjs.map((keypair) => ({
      address: keypair.address,
      name: keypair.name,
      secret: keypair.secret,
      balance: (keypair.balance && keypair.balance[networkId]) || "0",
    }));
    unsorted = unsorted.filter((item) => item.address && item);
    return unsorted.sort((a, b) => {
      if (!a.name || !b.name) {
        return 0;
      }
      return a.name.localeCompare(b.name);
    });
  }

  async loadAllKeypairs() {
    try {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, global-require, @typescript-eslint/no-var-requires
      const { networkManager } = require("~/ligo-components/eth-network");
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access
      const networkId: string = networkManager?.network?.id;
      const sorted = KeypairManager.getKeypairFromRedux(networkId);

      const updating = sorted.map(async (keypair) => {
        if (!networkId) return;
        const { address } = keypair;
        const { secret } = keypair;
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
        const account = await (networkManager?.sdk?.client?.getAccount(address) || {
          balance: "0.0",
        });
        redux.dispatch("UPDATE_KEYPAIR_BALANCE", {
          address,
          networkId,
          secret,
          // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access
          balance: account.balance,
        });
      });
      await Promise.all(updating)
        .then(() => {
          const keypairs = KeypairManager.getKeypairFromRedux(networkId);
          const event = new CustomEvent("updated", { detail: keypairs });
          this.eventTarget.dispatchEvent(event);
        })
        .catch((e) => {
          // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
          if (e?.errorDetail?.message) {
            // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
            notification.error("Balance fetching error", e.errorDetail.message);
          } else {
            console.error(JSON.stringify(e));
          }
        });
      return sorted;
    } catch (e) {
      console.error(e);
      return [];
    }
  }

  static newKeypair(kp: KpType, secretType: "privkey" | "mnemonic") {
    return kp.newKeypair(secretType);
  }

  async loadAndUpdateKeypairs() {
    const keypairs = await this.loadAllKeypairs();

    keypairs.forEach((k) => {
      this.keypairNames[k.address] = k.name;
    });

    const event = new CustomEvent("updated", { detail: keypairs });
    this.eventTarget.dispatchEvent(event);
  }

  async saveKeypair(name: string, keypair: Keypair) {
    redux.dispatch("UPDATE_KEYPAIR", { address: keypair.address, secret: keypair.secret, name });
    await this.loadAndUpdateKeypairs();
  }

  async updateKeypairName(address: string, name: string) {
    redux.dispatch("UPDATE_KEYPAIR", { address, name });
    await this.loadAndUpdateKeypairs();
  }

  async deleteKeypair(keypair: Keypair) {
    redux.dispatch("REMOVE_KEYPAIR", { address: keypair.address });
    await this.loadAndUpdateKeypairs();
  }

  getName(address: string) {
    return this.keypairNames[address];
  }

  async getKeypair(address: string) {
    const keypairs = await this.loadAllKeypairs();
    const keypair = keypairs.find((k) => k.address === address);
    if (!keypair) {
      throw new Error(`No keypair for <b>${address}</b>`);
    }
    return keypair;
  }

  async getSecret(
    address: string,
    key: keyof {
      address: string;
      name: string;
      secret: string;
      balance: string;
    } = "secret"
  ) {
    const keypair = await this.getKeypair(address);
    if (!keypair[key]) {
      throw new Error(`No ${key} for <b>${address}</b>`);
    }
    return keypair[key];
  }

  // async signTransaction(id, tx) {
  //   try {
  //     const modified = await this.signReqModal.current?.openModal(tx);
  //     this.channel.invoke("callback", id, null, modified);
  //   } catch {
  //     this.channel.invoke("callback", id, "User rejected the transaction.");
  //   }
  // }
}

export default new KeypairManager();
