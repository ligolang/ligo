/* eslint-disable @typescript-eslint/no-unsafe-assignment */
/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/naming-convention */
/* eslint-disable max-classes-per-file */
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
      // eslint-disable-next-line no-empty
    } catch (e) {}
  };

  set = () => {
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
      // eslint-disable-next-line @typescript-eslint/no-unused-expressions
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
  setFileSystem = (filesystems: fileSystem[]): void => {
    for (let i = 0; i < filesystems.length; i++) {
      if (filesystems[i] && this.fileSystems[filesystems[i].name]) {
        this.fileSystems[filesystems[i].name].set();
        return;
      }
    }
    throw new Error("No file system.");
  };
}
