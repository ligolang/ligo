import { getCachingKeys, dropByCacheKey } from "react-router-cache-route";
import platform from "~/base-components/platform";
import headerActions from "~/ligo-components/eth-header";
import notification from "~/base-components/notification";
import redux from "~/base-components/redux";

class NetworkManager {
  constructor() {
    this.networks = [];

    this._sdk = null;
    this.network = undefined;
    this.networks = [];
    this.Sdks = new Map();
  }

  addSdk(Sdk, networks) {
    networks.forEach(n => this.Sdks.set(n.id, Sdk));
    this.networks = [...this.networks, ...networks];

    const enabled = !process.env.REACT_APP_DISABLE_BROWSER_EXTENSION;
    if (platform.isWeb && enabled && Sdk.InitBrowserExtension) {
      this.browserExtension = Sdk.InitBrowserExtension(this);
    }
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
    return this.networks.find(n => n.id === this.networkId);
  }

  get symbol() {
    return this.current?.symbol;
  }

  newSdk(params) {
    const networkId = params.id.split(".")[0];
    const Sdk = this.Sdks.get(networkId);
    if (!Sdk) {
      return null;
    }
    return new Sdk(params);
  }

  async updateSdk(params) {
    this._sdk = this.newSdk({ ...this.network, ...params });
    await new Promise(resolve => {
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

  async setNetwork(network, { force, redirect = true, notify = true } = {}) {
    redux.dispatch("ACTIVE_CUSTOM_NETWORK", network);
    if (window.ethereum && window.ethereum.isConnected() && network.chainId) {
      const hexChainId = `0x${network.chainId.toString(16)}`;
      if (window.ethereum.chainId !== hexChainId) {
        try {
          await window.ethereum.request({
            method: "wallet_switchEthereumChain",
            params: [
              {
                chainId: hexChainId,
              },
            ],
          });
        } catch (e) {
          if (e.code === 4902) {
            await window.ethereum.request({
              method: "wallet_addEthereumChain",
              params: [
                {
                  chainId: hexChainId,
                  chainName: network.fullName,
                  rpcUrls: [network.url],
                },
              ],
            });
            await window.ethereum.request({
              method: "wallet_switchEthereumChain",
              params: [
                {
                  chainId: hexChainId,
                },
              ],
            });
          }
        }
      }
    }

    if (this.browserExtension && !force) {
      if (redux.getState().network) {
        notification.info(`Please use ${this.browserExtension.name} to switch the network.`);
      }
      return;
    }

    if (!network || network.id === redux.getState().network) {
      return;
    }

    if (process.env.DEPLOY === "bsn" && network.projectKey) {
      notification.warning(
        `${network.name}`,
        `The current network ${network.name} enables a project key, please turn it off in the BSN portal.`,
        5
      );
    }

    const cachingKeys = getCachingKeys();
    cachingKeys
      .filter(key => key.startsWith("contract-") || key.startsWith("account-"))
      .forEach(dropByCacheKey);

    this.network = network;
    if (network.id && network.id !== "dev") {
      try {
        this._sdk = this.newSdk(network);
      } catch (error) {
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

  async updateCustomNetwork({ url, option = "{}", notify = true }) {
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
      notification.success("Network Connected", `Connected to network at <b>${url}</b>`);
    }

    return info;
  }

  async createSdk(params) {
    const sdk = this.newSdk(params);
    try {
      const info = await sdk.networkInfo();
      this._sdk = sdk;
      return info;
    } catch (e) {
      console.warn(e);
      notification.error("Invalid Node URL", "");
    }
  }
}

export default new NetworkManager();
