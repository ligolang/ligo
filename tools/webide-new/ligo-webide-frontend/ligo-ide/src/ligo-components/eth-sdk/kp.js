import { ethers } from "ethers";
import { IpcChannel } from "~/base-components/ipc";
import utils from "./utils";

const channel = new IpcChannel("keypair");

export default {
  async newKeypair(_, secretType) {
    const secret = await channel.invoke("post", "new-secret");
    if (secretType === "mnemonic") {
      const extraEntropy = utils.format.bytesFromHex(secret);
      const wallet = ethers.Wallet.createRandom({ extraEntropy });
      return {
        address: wallet.address.toLowerCase(),
        secret: wallet.mnemonic.phrase,
        secretName: "Mnemonic",
      };
    }
    const address = ethers.utils.computeAddress(secret);
    return {
      address: address.toLowerCase(),
      secret,
      secretName: "Private Key",
    };
  },
  importKeypair(secretParam) {
    let secret = secretParam;
    if (secret.startsWith("0x") || /^[0-9a-zA-Z]{64}$/.test(secret)) {
      if (!secret.startsWith("0x")) {
        secret = `0x${secret}`;
      }
      const address = ethers.utils.computeAddress(secret);
      return {
        address: address.toLowerCase(),
        secret,
        secretName: "Private Key",
      };
    }
    const wallet = ethers.Wallet.fromMnemonic(secret);
    return {
      address: wallet.address.toLowerCase(),
      secret: wallet.mnemonic.phrase,
      secretName: "Mnemonic",
    };
  },
  walletFrom(secret) {
    if (secret.startsWith("0x")) {
      return new ethers.Wallet(secret);
    }
    return ethers.Wallet.fromMnemonic(secret);
  },
};
