import { InMemorySigner } from "@taquito/signer";
import { b58cencode, prefix } from "@taquito/utils";
import * as Bip39 from "bip39";
import * as Ed25519 from "ed25519-hd-key";

export default {
  async newKeypair(secretType: "privkey" | "mnemonic") {
    const mnemonic = Bip39.generateMnemonic(128);
    const seed = Bip39.mnemonicToSeedSync(mnemonic);
    const accPrivateKey = b58cencode(
      Ed25519.derivePath("m/44'/1729'/0'/0'", seed.toString("hex")).key.slice(0, 32),
      prefix.edsk2
    );
    const signer = await InMemorySigner.fromSecretKey(accPrivateKey);
    const accAddress = await signer.publicKeyHash();
    return {
      address: accAddress,
      secret: secretType === "mnemonic" ? mnemonic : accPrivateKey,
      secretName: secretType === "mnemonic" ? "Mnemonic" : "Private Key",
    };
  },
  async importKeypair(secretParam: string) {
    let secretType = "secretkey";
    let accPrivateKey = secretParam;
    if (accPrivateKey.includes(" ")) {
      secretType = "mnemonic";
      accPrivateKey = this.secretFromMnemonic(secretParam);
    }
    const signer = await InMemorySigner.fromSecretKey(accPrivateKey);
    const accAddress = await signer.publicKeyHash();
    return {
      address: accAddress,
      secret: secretParam,
      secretName: secretType === "mnemonic" ? "Mnemonic" : "Private Key",
    };
  },
  secretFromMnemonic(mnemonic: string) {
    const seed = Bip39.mnemonicToSeedSync(mnemonic);
    return b58cencode(
      Ed25519.derivePath("m/44'/1729'/0'/0'", seed.toString("hex")).key.slice(0, 32),
      prefix.edsk2
    );
  },
};
