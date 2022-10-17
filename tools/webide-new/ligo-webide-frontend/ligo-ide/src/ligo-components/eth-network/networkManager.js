import { getCachingKeys, dropByCacheKey } from "react-router-cache-route";
import { BeaconWallet } from "@taquito/beacon-wallet";
import platform from "~/base-components/platform";
import headerActions from "~/ligo-components/eth-header";
import notification from "~/base-components/notification";
import redux from "~/base-components/redux";
import { BrowserExtension } from "~/ligo-components/eth-sdk";

class NetworkManager {
  constructor() {
    this.networks = [];

    this._sdk = null;
    this.network = undefined;
    this.networks = [];
    this.Sdks = new Map();
    this.isWallet = false;
    this.browserExtension = new BrowserExtension(new BeaconWallet({ name: "LIGO Web IDE" }));
  }

  addSdk(Sdk, networks) {
    networks.forEach((n) => this.Sdks.set(n.id, Sdk));
    this.networks = [...this.networks, ...networks];
  }

  get networkId() {
    return this.network?.id;
  }

  get Sdk() {
    return this.Sdks.get(this.networkId);
  }

  get sdk() {
    return this._sdk;
  }

  get current() {
    return this.networks.find((n) => n.id === this.networkId);
  }

  get symbol() {
    return this.current?.symbol;
  }

  get customNetWorks() {
    return redux.getState().customNetworks.toJS();
  }

  addNetworks(networks) {
    networks.forEach((n) => this.Sdks.set(n.id, this.Sdk || this.Sdks.get("custom")));
    this.networks = networks;
  }

  newSdk(params) {
    const networkId = params.id;
    const Sdk = this.Sdks.get(networkId);
    if (!Sdk) {
      return null;
    }
    const sdksdk = new Sdk(params, this.browserExtension);
    return sdksdk;
  }

  async updateSdk(params) {
    this._sdk = this.newSdk({ ...this.network, ...params });
    await new Promise((resolve) => {
      const h = setInterval(() => {
        if (!this.sdk) {
          clearInterval(h);
          return;
        }
        this.sdk
          .getStatus()
          .then(() => {
            clearInterval(h);
            resolve();
          })
          .catch(() => null);
      }, 1000);
    });
  }

  async disposeSdk(params) {
    this._sdk && this._sdk.dispose();
    if (this.networkId === "dev") {
      this._sdk = null;
    }
    if (this.onSdkDisposedCallback) {
      this.onSdkDisposedCallback();
    }
  }

  onSdkDisposed(callback) {
    this.onSdkDisposedCallback = callback;
  }

  hasDuplicatedNetwork(rpcUrl) {
    return !this.networks.every((net) => net.url !== rpcUrl);
  }

  deleteNetwork(networkId) {
    const index = this.networks.findIndex((n) => n.id === networkId);
    if (index === -1) {
      return;
    }
    this.networks.splice(index, 1);
    this.Sdks.delete(networkId);
  }

  async setNetwork(network, { force, redirect = true, notify = true } = {}) {
    redux.dispatch("ACTIVE_CUSTOM_NETWORK", network);
    if (this.browserExtension && network.chainId && this.isWallet) {
      const chain = await this.browserExtension.getChain();
      if (!chain || `${chain.name}+${chain.rpcUrl}` !== network.chainId) {
        this.browserExtension.ethereum.requestPermissions({
          network: {
            name: network.fullName,
            rpcUrl: network.url,
            type: network.type ? network.type : "custom",
          },
        });
        // try {
        //   await window.ethereum.request({
        //     method: "wallet_switchEthereumChain",
        //     params: [
        //       {
        //         chainId: hexChainId,
        //       },
        //     ],
        //   });
        // } catch (e) {
        //   if (e.code === 4902) {
        //     await window.ethereum.request({
        //       method: "wallet_addEthereumChain",
        //       params: [
        //         {
        //           chainId: hexChainId,
        //           chainName: network.fullName,
        //           rpcUrls: [network.url],
        //         },
        //       ],
        //     });
        //     await window.ethereum.request({
        //       method: "wallet_switchEthereumChain",
        //       params: [
        //         {
        //           chainId: hexChainId,
        //         },
        //       ],
        //     });
        //   }
        // }
      }
    }
    // if (this.browserExtension && !force) {
    //   if (redux.getState().network) {
    //     notification.info(`Please use ${this.browserExtension.name} to switch the network.`);
    //   }
    //   return;
    // }
    if (!network || (network.id === redux.getState().network && this._sdk)) {
      return;
    }
    // if (process.env.DEPLOY === "bsn" && network.projectKey) {
    //   notification.warning(
    //     `${network.name}`,
    //     `The current network ${network.name} enables a project key, please turn it off in the BSN portal.`,
    //     5
    //   );
    // }
    const cachingKeys = getCachingKeys();
    cachingKeys
      .filter((key) => key.startsWith("contract-") || key.startsWith("account-"))
      .forEach(dropByCacheKey);
    this.network = network;
    if (network.id && network.id !== "dev") {
      try {
        this._sdk = this.newSdk(network);
      } catch (error) {
        console.log(error);
        this._sdk && this._sdk.dispose();
        this._sdk = null;
      }
    } else {
      this._sdk && this._sdk.dispose();
      this._sdk = null;
    }
    redux.dispatch("SELECT_NETWORK", network.id);
    if (notify) {
      notification.success("Network", network.notification);
    }
    if (redirect) {
      headerActions.updateNetwork(network.id);
    }
  }

  async updateCustomNetwork({ url, option = "{}", notify = true, name, chainId = "" }) {
    try {
      if (option) {
        option = JSON.parse(option);
      }
    } catch {
      notification.error("Invalid Option", "");
      return;
    }
    const info = await this.createSdk({ id: "custom", url, option });

    if (info && notify) {
      redux.dispatch("SELECT_NETWORK", "custom");
      redux.dispatch("CHANGE_NETWORK_STATUS", true);
      notification.success("Network Connected", `Connected to network at <b>${url}</b>`);
    }

    return info;
  }

  async createSdk(params) {
    const sdk = this.newSdk(params);
    try {
      const info = await sdk.networkInfo();
      if (params.id !== "custom") this._sdk = sdk;
      return info;
    } catch (e) {
      console.warn(e);
      notification.error("Invalid Node URL", "");
    }
  }

  findChainById(value) {
    return this.networks.find((net) => net.id === value || net.name === value);
  }

  getNewNetList() {
    const customNetworkGroup = Object.keys(this.customNetWorks)
      .map((name) => ({
        group: "others",
        icon: "fas fa-vial",
        id: name,
        networkId: this.customNetWorks[name]?.networkId || name,
        name,
        fullName: name,
        notification: `Switched to <b>${name}</b>.`,
        url: this.customNetWorks[name].url,
        chainId: this.customNetWorks[name]?.chainId || "",
      }))
      .sort((a, b) => a.name.localeCompare(b.name));

    return this.networks
      .filter((item) => item.group !== "others" || item.id === "others")
      .concat([
        {
          fullName: "Custom Network",
          group: "others",
          icon: "fas fa-edit",
          id: "custom",
          name: "Custom",
          notification: "Switched to <b>Custom</b> network.",
          symbol: "ETH",
          url: "",
        },
      ])
      .concat(customNetworkGroup);
  }
}

export default new NetworkManager();
