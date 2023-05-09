import makeSdk from "./makeSdk";

import kp from "./kp";
import networks, { customNetworks } from "./networks";

import EthersClient from "./EthersClient";
import EthTxManager from "./EthTxManager";

export default makeSdk({
  kp,
  networks,
  customNetworks,
  Client: EthersClient,
  TxManager: EthTxManager,
});

export { makeSdk, kp, EthersClient, EthTxManager };
export { default as BrowserExtension } from "./BrowserExtension";
