import { ethers } from "ethers";
import Big from "big.js";

import txOptions from "./txOptions";

const display = value => {
  const amount = ethers.utils.formatEther(value);
  if (amount > 0.001) {
    return `${new Intl.NumberFormat().format(amount)} ETH`;
  }
  if (amount > 0.0000000001) {
    const gvalue = ethers.utils.formatUnits(value, "gwei");
    return `${new Intl.NumberFormat().format(gvalue)} Gwei`;
  }
  return `${new Intl.NumberFormat().format(value)} wei`;
};

const utf8 = hex => {
  try {
    return ethers.utils.toUtf8String(hex);
  } catch {
    return "";
  }
};

function parseObject(values, abi, chainId) {
  const parsedOutputs = abi.map((param, index) => {
    const value = values[index];
    const { name, type, internalType } = param;
    const parsed = parseValue.call(this, value, param, chainId);
    const result = { type, internalType, value: parsed };
    return [name || `(${index})`, result];
  });
  return Object.fromEntries(parsedOutputs);
}

function parseValue(value, param, chainId) {
  const { type, internalType, components } = param;
  if (type === "tuple") {
    return parseObject.call(this, value, components, chainId);
  }
  if (type.endsWith("]")) {
    const itemParam = {
      type: type.replace(/\[\d*\]/, ""),
      internalType: internalType.replace(/\[\d*\]/, ""),
      components,
    };
    return value.map(v => {
      const parsed = parseValue.call(this, v, itemParam, chainId);
      return {
        value: parsed,
        type: itemParam.type,
        internalType: itemParam.internalType,
      };
    });
  }
  if (type.startsWith("uint") || type.startsWith("int")) {
    return value.toString();
  }
  if (type.startsWith("byte")) {
    return { hex: ethers.utils.hexlify(value), utf8: utf8(value) };
  }
  if (type.startsWith("address")) {
    return this.formatAddress(value, chainId);
  }
  return value;
}

export default {
  txOptions,
  isValidAddress: address => ethers.utils.isAddress(address),
  formatAddress: address =>
    ethers.utils.isAddress(address) ? ethers.utils.getAddress(address) : "--",
  abbreviateAddress: addr => {
    let address = addr;
    address = ethers.utils.isAddress(address) ? ethers.utils.getAddress(address) : "--";
    return `${address.substr(0, 12)}...${address.substr(address.length - 6, address.length)}`;
  },
  simplifyAddress: address => (address ? address.toLowerCase() : ""),
  isValidAddressReturn: address =>
    ethers.utils.isAddress(address) ? ethers.utils.getAddress(address) : address,
  sign: {
    sha3: ethers.utils.keccak256,
  },
  format: {
    big: value => Big(value),
    bytes: str => ethers.utils.toUtf8Bytes(str),
    utf8,
    bytesFromHex: hex => ethers.utils.arrayify(hex),
  },
  unit: {
    fromValue: ethers.utils.formatEther,
    toValue: ethers.utils.parseEther,
    valueToGvalue: v => ethers.utils.formatUnits(v, "gwei"),
  },
  display,
  decodeError: () => "",
  parseError: e => {
    e.reason = "";
    try {
      const body = e.body || e.error.body || e.error.error.body;
      const res = JSON.parse(body);
      e.reason = res.error.message;
    } catch {
      return e;
    }
    return e;
  },
  parseObject,
  parseValue,
};
