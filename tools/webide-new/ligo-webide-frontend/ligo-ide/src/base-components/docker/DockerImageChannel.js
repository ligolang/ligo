import semver from "semver";
import { IpcChannel } from "~/base-components/ipc";

const defaultFilter = v => semver.valid(v);
const defaultSort = (v1, v2) => (semver.lt(v1, v2) ? 1 : -1);

const getHandler = (opt, defaultHandler) => {
  if (typeof opt === "boolean") {
    return opt ? defaultHandler : undefined;
  }
  return opt || defaultHandler;
};

export default class DockerImageChannel extends IpcChannel {
  constructor(imageName, opts = {}) {
    super("docker-image", imageName);
    this.eventTarget = new EventTarget();

    this.filter = getHandler(opts.filter, defaultFilter);
    this.sort = getHandler(opts.sort, defaultSort);
    this.size = opts.size || 20;
  }

  get imageName() {
    return this.uid;
  }

  async installed() {
    return await this.invoke("any");
  }

  async versions() {
    let versions = [];
    try {
      versions = await this.invoke("versions");
    } catch (e) {}
    versions = this._organizeVersionsByKey(versions, "Tag");
    const event = new CustomEvent("versions", { detail: versions });
    this.eventTarget.dispatchEvent(event);
    return versions;
  }

  onVersionsRefreshed(callback) {
    const eventHandler = event => callback(event.detail);
    this.eventTarget.addEventListener("versions", eventHandler);
  }

  async remoteVersions(size = this.size) {
    const versions = await this.invoke("remoteVersions", size);
    return this._organizeVersionsByKey(versions, "name").slice(0, size);
  }

  _organizeVersionsByKey(versions, key) {
    let organized = [...versions];
    if (this.filter) {
      organized = organized.filter(v => this.filter(v[key]));
    }
    if (this.sort) {
      organized = organized.sort((x, y) => this.sort(x[key], y[key]));
    }
    return organized;
  }

  async delete(version) {
    return await this.invoke("deleteVersion", version);
  }
}
