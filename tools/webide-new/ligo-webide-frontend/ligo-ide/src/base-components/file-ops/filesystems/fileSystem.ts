export class fileSystem {
  name!: string; // TODO remove !

  enabled: boolean;

  available: boolean;

  fs: any;

  fsCallBack: any;

  hasWorkSpaces: boolean;

  loaded: boolean;

  load!: () => Promise<unknown>; // TODO remove !

  test!: () => Promise<unknown>; // TODO remove !

  constructor() {
    this.available = false;
    this.enabled = false;
    this.hasWorkSpaces = false;
    this.loaded = false;
  }

  checkWorkspaces = async () => {
    try {
      await this.fs.stat(".workspaces");
      this.hasWorkSpaces = true;
    } catch (e) {}
  };

  set = async () => {
    const w = window as any;
    if (!this.loaded) {
      return false;
    }
    w.ligoIdeFileSystem = this.fs;
    w.ligoIdeFileSystem.name = this.name;
    w.ligoIdeFileSystemCallback = this.fsCallBack;
    return true;
  };
}

export class fileSystems {
  fileSystems: Record<string, fileSystem>;

  constructor() {
    this.fileSystems = {};
  }

  addFileSystem = async (fs: fileSystem): Promise<boolean> => {
    try {
      this.fileSystems[fs.name] = fs;
      (await fs.test()) && (await fs.load());
      console.log(`${fs.name} is loaded...`);
      return true;
    } catch (e) {
      console.log(`${fs.name} not available...`);
      return false;
    }
  };

  /**
   * sets filesystem using list as fallback
   * @param {string[]} names
   * @returns {Promise}
   */
  setFileSystem = async (filesystems: fileSystem[]): Promise<void> => {
    for (const fs of filesystems) {
      if (fs && this.fileSystems[fs.name]) {
        await this.fileSystems[fs.name].set();
        return;
      }
    }
    throw new Error("No file system.");
  };
}
