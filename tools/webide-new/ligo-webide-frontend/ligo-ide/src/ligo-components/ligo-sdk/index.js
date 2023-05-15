import makeSdk from "./makeSdk";

import kp from "./kp";
import networks, { customNetworks } from "./networks";

import LigoClient from "./LigoClient";
import LigoTxManager from "./LigoTxManager";

export default makeSdk({
  kp,
  networks,
  customNetworks,
  Client: LigoClient,
  TxManager: LigoTxManager,
});

export { makeSdk, kp, LigoClient, LigoTxManager };
export { default as BrowserExtension } from "./BrowserExtension";
