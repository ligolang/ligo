/* eslint-disable @typescript-eslint/unbound-method */
/* eslint-disable max-classes-per-file */
/* eslint-disable @typescript-eslint/no-unused-vars */
import LightningFS from "@isomorphic-git/lightning-fs";
import { fileSystem } from "./fileSystem";

export type ExtendedFs = {
  exists: (path: string) => Promise<boolean>;
  rmdir: (path: string) => Promise<void>;
  readdir: (path: string) => Promise<string[]>;
  unlink: (path: string) => Promise<void>;
  mkdir: (path: string) => Promise<void>;
  readFile: (path: string) => Promise<string>;
  rename: (from: string, to: string) => Promise<void>;
  writeFile: (path: string, content: string) => Promise<void>;
  stat: (path: string) => Promise<LightningFS.Stats>;
  init(name: string, opt?: LightningFS.Options): void;
  lstat(filePath: string): Promise<LightningFS.Stats>;
  readlink(filePath: string): Promise<string>;
  symlink(target: string, filePath: string): Promise<void>;
};

export class IndexedDBStorage extends LightningFS {
  base: LightningFS.PromisifiedFS;

  addSlash: (file: string) => string;

  extended: ExtendedFs;

  constructor(name: string) {
    super(name);
    this.addSlash = (file) => {
      // eslint-disable-next-line no-param-reassign
      if (!file.startsWith("/")) file = `/${file}`;
      return file;
    };
    this.base = this.promises;
    this.extended = {
      ...this.promises,
      init: this.promises.init,
      lstat: this.promises.lstat,
      readlink: this.promises.readlink,
      symlink: this.promises.symlink,
      exists: async (path: string) => {
        return new Promise((resolve) => {
          this.base
            .stat(this.addSlash(path))
            .then(() => resolve(true))
            .catch(() => resolve(false));
        });
      },
      rmdir: async (path: string) => {
        return this.base.rmdir(this.addSlash(path));
      },
      readdir: async (path: string) => {
        return this.base.readdir(this.addSlash(path));
      },
      unlink: async (path: string) => {
        return this.base.unlink(this.addSlash(path));
      },
      mkdir: async (path: string) => {
        return this.base.mkdir(this.addSlash(path));
      },
      readFile: async (path: string) => {
        return this.base
          .readFile(this.addSlash(path), "utf8")
          .then((content: string | Uint8Array) =>
            typeof content === "string" ? content : content.toString()
          );
      },
      rename: async (from: string, to: string) => {
        return this.base.rename(this.addSlash(from), this.addSlash(to));
      },
      writeFile: async (path: string, content: string) => {
        return this.base.writeFile(this.addSlash(path), content, "utf8");
      },
      stat: async (path: string) => {
        return this.base.stat(this.addSlash(path));
      },
    };
  }
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export class indexedDBFileSystem extends fileSystem {
  constructor() {
    super();
    this.name = "indexedDB";
  }

  load = async () => {
    return new Promise((resolve, reject) => {
      try {
        const fs = new IndexedDBStorage("LigoIdeFileSystem");
        fs.init("LigoIdeFileSystem");
        this.fs = fs.extended;
        this.fsCallBack = fs;
        this.loaded = true;
        resolve(true);
      } catch (e) {
        reject(e);
      }
    });
  };

  test = async () => {
    return new Promise((resolve, reject) => {
      if (!window.indexedDB) {
        this.available = false;
        reject(new Error("No indexedDB on window"));
      }
      const request = window.indexedDB.open("LigoIdeTestDataBase");
      request.onerror = () => {
        this.available = false;
        reject(new Error("Error creating test database"));
      };
      request.onsuccess = () => {
        window.indexedDB.deleteDatabase("LigoIdeTestDataBase");
        this.available = true;
        resolve(true);
      };
    });
  };
}
