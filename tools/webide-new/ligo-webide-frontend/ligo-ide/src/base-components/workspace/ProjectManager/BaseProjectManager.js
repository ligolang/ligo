import { IpcChannel } from "~/base-components/ipc";

export default class BaseProjectManager {
  static ProjectSettings = null;

  static channel = new IpcChannel("project");

  static terminalButton = null;

  static instance = null;

  constructor(project, projectRoot) {
    BaseProjectManager.instance = this;
    this.project = project;
    this.projectRoot = projectRoot;
  }

  dispose() {}

  get settingsFilePath() {
    throw new Error("ProjectManager.settingsFilePath is not implemented.");
  }

  onRefreshDirectory(callback) {
    BaseProjectManager.channel.on("refresh-directory", callback);
  }

  offRefreshDirectory() {
    BaseProjectManager.channel.off("refresh-directory");
  }

  refreshDirectory() {}

  async readDirectoryRecursively(folderPath, stopCriteria = (child) => child.type === "file") {
    const children = await this._readDirectoryRecursively(folderPath, stopCriteria);
    return children.map((child) => {
      child.relative = this.path.relative(folderPath, child.path);
      return child;
    });
  }

  async _readDirectoryRecursively(folderPath, stopCriteria) {
    const children = await this.readDirectory(folderPath);
    const traversed = await Promise.all(
      children.map(async (child) => {
        if (stopCriteria(child)) {
          return child;
        }
        if (child.type === "file") {
          return;
        }
        return this._readDirectoryRecursively(child.path, stopCriteria);
      })
    );
    return traversed.flat().filter(Boolean);
  }

  static effect(evt, callback) {
    return () => {
      const dispose = BaseProjectManager.channel.on(evt, callback);
      BaseProjectManager.channel.trigger("current-value", evt);
      return dispose;
    };
  }
}
