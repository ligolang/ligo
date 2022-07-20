export default class IndexedLocalFs {
  static stat(path: string) {
    return {
      isDirectory: async (): Promise<boolean> =>
        (await window.ligoIdeFileSystem.stat(path)).isDirectory(),
      isFile: async (): Promise<boolean> => (await window.ligoIdeFileSystem.stat(path)).isFile(),
    };
  }

  static async exists(path: string): Promise<boolean> {
    return window.ligoIdeFileSystem.exists(path);
  }

  static async readDirectory(path: string): Promise<{ [a: string]: { isDirectory: boolean } }> {
    const files = await window.ligoIdeFileSystem.readdir(path);
    const trailPath = path.replace(/^\/|\/$/g, "");
    const result: { [a: string]: { isDirectory: boolean } } = {};
    if (files) {
      for (const file of files) {
        const trailFile = file.replace(/^\/|\/$/g, "");
        const absPath = `${trailPath}/${trailFile}`;
        result[absPath] = {
          // eslint-disable-next-line no-await-in-loop
          isDirectory: await IndexedLocalFs.stat(absPath).isDirectory(),
        };
      }
    }
    return result;
  }

  static async readFile(filePath: string): Promise<string> {
    return window.ligoIdeFileSystem.readFile(filePath);
  }

  static async writeDirectory(path: string): Promise<void> {
    const paths = path.split("/");
    if (paths.length && paths[0] === "") {
      paths.shift();
    }

    let curDir = "";
    for (const value of paths) {
      curDir = `${curDir}/${value}`;
      // eslint-disable-next-line no-await-in-loop
      if (!(await IndexedLocalFs.exists(curDir))) {
        // eslint-disable-next-line no-await-in-loop
        await window.ligoIdeFileSystem.mkdir(curDir);
      }
    }
  }

  static async writeFile(filePath: string, content: string): Promise<void> {
    await IndexedLocalFs.writeDirectory(filePath.substring(0, filePath.lastIndexOf("/")));
    await window.ligoIdeFileSystem.writeFile(filePath, content);
  }

  static async deleteFile(filePath: string): Promise<void> {
    await window.ligoIdeFileSystem.unlink(filePath);
  }

  static async deleteDirectory(dirPath: string): Promise<void> {
    await window.ligoIdeFileSystem.rmdir(dirPath);
  }

  static async rename(oldPath: string, newPath: string): Promise<void> {
    await window.ligoIdeFileSystem.rename(oldPath, newPath);
  }
}
