import notification from "~/base-components/notification";
import { BaseQueueManager } from "~/base-components/queue";

class Queue extends BaseQueueManager {
  async process(pendingTransaction, txHash, data, callbacks) {
    this.updateStatus(txHash, "PUSHING", data, callbacks);
    if (data.contractName) {
      notification.info("Deploying...", `Deploying contract <b>${data.contractName}</b>...`);
    } else {
      notification.info("Pushing transaction...", `Transaction hash <b>${txHash}</b>...`);
    }

    let res;
    try {
      res = await pendingTransaction.mined();
    } catch (e) {
      console.warn(e);
      this.updateStatus(txHash, "FAILED-TIMEOUT", { error: { message: e.message } }, callbacks);
      notification.error("Transaction Timeout", e.message);
      return;
    }
    if (res && res.error) {
      notification.error("Transaction Failed", res.error);

      this.updateStatus(
        txHash,
        "FAILED",
        {
          tx: res.tx,
          receipt: res.receipt,
          error: { code: res.code, message: res.error },
        },
        callbacks
      );
      return;
    }
    this.updateStatus(txHash, "MINED", res, callbacks);

    let receipt;
    try {
      receipt = await pendingTransaction.executed();
    } catch (e) {
      console.warn(e);
      notification.error("Transaction Failed", e.message);
      this.updateStatus(
        txHash,
        "FAILED",
        {
          receipt: e.receipt,
          error: { code: e.code, message: e.message, data: e.data },
        },
        callbacks
      );
      return;
    }
    if (receipt.gasUsed) {
      const gasUsed = receipt.gasUsed.toString();
      // const gasFee = receipt.gasFee.toString()
      notification.info("Transaction Executed", `Gas used ${gasUsed}.`);
      // notification.info('Transaction Executed', `Gas used ${gasUsed}, gas fee ${gasFee}`)
    }
    this.updateStatus(txHash, "EXECUTED", { receipt }, callbacks);

    let result;
    try {
      result = await pendingTransaction.confirmed();
    } catch (e) {
      this.updateStatus(txHash, "CONFIRMED", {}, callbacks);
      return;
    }
    notification.success("Transaction Confirmed", result?.message || "");
    this.updateStatus(txHash, "CONFIRMED", { confirmed: result?.message }, callbacks);
  }
}

export default new Queue();
