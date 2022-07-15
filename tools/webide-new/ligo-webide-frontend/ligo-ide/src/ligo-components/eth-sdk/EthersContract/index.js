import { ethers } from "ethers";
import utils from "../utils";

export default class EthersContract {
  constructor({ address, abi }, client) {
    this.address = address;
    this.abi = abi;
    this.provider = client.provider;
    this.instance = new ethers.Contract(address, abi, client.provider);
  }

  async query(method, { array }) {
    let result;
    try {
      result = await this.instance.functions[method](...array);
    } catch (e) {
      throw utils.parseError(e);
    }
    return this.parseResult(result, method);
  }

  async execute(method, { array }, override) {
    try {
      const tx = await this.instance.populateTransaction[method](...array, override);
      const voidSigner = new ethers.VoidSigner(override.from, this.provider);
      const populated = await voidSigner.populateTransaction(tx);
      const nonce = await this.provider.getTransactionCount(tx.from);
      populated.nonce = nonce;
      return {
        tx: populated,
        getResult: async (tx, height) => {
          const data = await this.provider.call(tx, height);
          const result = this.instance.interface.decodeFunctionResult(method, data);
          return this.parseResult(result, method);
        },
      };
    } catch (e) {
      console.warn(e);
      throw utils.parseError(e);
    }
  }

  parseResult(result, method) {
    const methodAbi = this.abi.find(item => item.name === method);
    const abi = methodAbi && methodAbi.outputs;
    const parsed = utils.parseObject(result, abi);
    return {
      raw: result,
      parsed: Object.values(parsed),
    };
  }

  get maxGap() {
    return 100;
  }

  async getLogs(event, { from, to }) {
    const filter = this.instance.filters[event.name]();
    try {
      const logs = await this.instance.queryFilter(filter, from, to);
      return logs;
    } catch (e) {
      throw utils.parseError(e);
    }
  }
}
