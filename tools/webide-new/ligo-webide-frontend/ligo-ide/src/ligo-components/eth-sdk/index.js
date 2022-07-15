import makeSdk from "./makeSdk";

import kp from "./kp";
import networks, { customNetworks } from "./networks";

import EthersClient from "./EthersClient";
import EthersContract from "./EthersContract";
import EthTxManager from "./EthTxManager";

import BrowserExtension from "./BrowserExtension";

import utils from "./utils";
import rpc from "./rpc";

export default makeSdk({
  kp,
  networks,
  customNetworks,
  Client: EthersClient,
  Contract: EthersContract,
  TxManager: EthTxManager,
  BrowserExtension,
  utils,
  rpc,
});

export { makeSdk, kp, EthersClient, EthersContract, EthTxManager, utils, rpc };
export { default as redux } from "./redux";
