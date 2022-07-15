export default class IndexedLocalFs {
  stat(path: string) {
    return {
      isDirectory: async (): Promise<boolean> =>
        (await window.ligoIdeFileSystem.stat(path)).isDirectory(),
      isFile: async (): Promise<boolean> => (await window.ligoIdeFileSystem.stat(path)).isFile(),
    };
  }

  async exists(path: string): Promise<boolean> {
    return window.ligoIdeFileSystem.exists(path);
  }

  async readDirectory(path: string): Promise<{ [a: string]: { isDirectory: boolean } }> {
    const files = await window.ligoIdeFileSystem.readdir(path);
    const trailPath = path.replace(/^\/|\/$/g, "");
    const result: { [a: string]: { isDirectory: boolean } } = {};
    if (files) {
      for (const file of files) {
        const trailFile = file.replace(/^\/|\/$/g, "");
        const absPath = `${trailPath}/${trailFile}`;
        result[absPath] = {
          isDirectory: await this.stat(absPath).isDirectory(),
        };
      }
    }
    return result;
  }

  async readFile(filePath: string): Promise<string> {
    return window.ligoIdeFileSystem.readFile(filePath);
  }

  async writeDirectory(path: string): Promise<void> {
    const paths = path.split("/");
    if (paths.length && paths[0] === "") {
      paths.shift();
    }

    let curDir = "";
    for (const value of paths) {
      curDir = `${curDir}/${value}`;
      if (!(await this.exists(curDir))) {
        await window.ligoIdeFileSystem.mkdir(curDir);
      }
    }
  }

  async writeFile(filePath: string, content: string): Promise<void> {
    await this.writeDirectory(filePath.substring(0, filePath.lastIndexOf("/")));
    await window.ligoIdeFileSystem.writeFile(filePath, content);
  }

  async deleteFile(filePath: string): Promise<void> {
    await window.ligoIdeFileSystem.unlink(filePath);
  }

  async deleteDirectory(dirPath: string): Promise<void> {
    await window.ligoIdeFileSystem.rmdir(dirPath);
  }

  async rename(oldPath: string, newPath: string): Promise<void> {
    await window.ligoIdeFileSystem.rename(oldPath, newPath);
  }
}
