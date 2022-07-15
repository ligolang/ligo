import keypairManager from "~/base-components/keypair";

import kp from "../kp";

export default function signatureProvider(address) {
  return async tx => {
    const secret = await keypairManager.getSecret(address.toLowerCase());
    const wallet = kp.walletFrom(secret);
    return wallet.signTransaction(tx);
  };
}
