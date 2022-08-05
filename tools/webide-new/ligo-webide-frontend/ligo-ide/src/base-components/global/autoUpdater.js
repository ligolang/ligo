import { IpcChannel } from "~/base-components/ipc";
import globalModalManager from "./modals/globalModalManager";

class AutoUpdater {
  constructor() {
    this.channel = new IpcChannel("auto-update");
    this.channel.on("status", (status) => this.onStatus(status));
  }

  dispose() {
    this.channel.dispose();
  }

  check() {
    this.channel.invoke("check");
  }

  async onStatus(status) {
    switch (status.type) {
      case "update-downloaded":
        const { version } = status.info;
        const updateNow = await globalModalManager.openAutoUpdateModal(version);
        if (updateNow) {
          this.channel.invoke("updateNow");
        }
        break;
      default:
        console.debug(status);
    }
  }
}

export default new AutoUpdater();
