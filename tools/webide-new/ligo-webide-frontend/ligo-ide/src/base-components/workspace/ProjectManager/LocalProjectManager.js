import pathHelper from "path-browserify";
import fileOps from "~/base-components/file-ops";
import notification from "~/base-components/notification";
import { modelSessionManager } from "~/base-components/code-editor";
import { IpcChannel } from "~/base-components/ipc";

import { sortFile } from "./helper";
import { getExamples } from "./examples";

export default class LocalProjectManager {
  static ProjectSettings = null;

  static channel = new IpcChannel("project");

  static terminalButton = null;

  static instance = null;

  constructor(project, projectRoot) {
    LocalProjectManager.instance = this;
    this.project = project;
    this.projectRoot = projectRoot;
  }

  dispose() {}

  get settingsFilePath() {
    throw new Error("ProjectManager.settingsFilePath is not implemented.");
  }

  onRefreshDirectory(callback) {
    LocalProjectManager.channel.on("refresh-directory", callback);
  }

  offRefreshDirectory() {
    LocalProjectManager.channel.off("refresh-directory");
  }

  async readDirectoryRecursively(folderPath, stopCriteria = (child) => child.type === "file") {
    const children = await this._readDirectoryRecursively(folderPath, stopCriteria);
    return children.map((child) => {
      child.relative = pathHelper.relative(folderPath, child.path);
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
      const dispose = LocalProjectManager.channel.on(evt, callback);
      LocalProjectManager.channel.trigger("current-value", evt);
      return dispose;
    };
  }

  // /////////////////////////////////////////////////////////////////

  static async createProject(name, template) {
    return this.processProject(name, undefined, template);
  }

  static async openProject(obj, name) {
    return this.processProject(name, obj, undefined);
  }

  static async processProject(name, obj, template) {
    const data = {
      id: name,
      author: "local",
      path: `.workspaces/${name}`,
      name,
    };

    if (await fileOps.exists(data.path)) {
      throw new Error("workspace already exists");
    } else {
      await fileOps.writeDirectory(data.path);
    }

    if (obj) {
      for (const key of Object.keys(obj)) {
        try {
          await fileOps.writeFile(`${data.path}/${key}`, obj[key].content);
        } catch (error) {
          console.error(error);
        }
      }
    } else if (template) {
      const examples = getExamples(data.name, template);

      for (const file in examples) {
        try {
          await fileOps.writeFile(examples[file].name, examples[file].content);
        } catch (error) {
          console.error(error);
        }
      }
    }

    return data;
  }

  async prepareProject() {
    if (!(await fileOps.isDirectory(this.projectRoot))) {
      return { error: "invalid project" };
    }

    let projectSettings;
    try {
      projectSettings = await this.readProjectSettings();
    } catch (e) {
      console.warn(e);
      return {
        initial: {
          path: this.settingsFilePath,
          pathInProject: this.settingsFilePath,
        },
        projectSettings: null,
      };
    }

    if (await this.isMainValid()) {
      return {
        initial: { path: this.mainFilePath, pathInProject: this.mainFilePath },
        projectSettings,
      };
    }
    return {
      initial: {
        path: this.settingsFilePath,
        pathInProject: this.settingsFilePath,
      },
      projectSettings,
    };
  }

  pathForProjectFile(relativePath) {
    return this.projectRoot ? pathHelper.join(this.projectRoot, relativePath) : "";
  }

  pathInProject(filePath) {
    return pathHelper.relative(this.projectRoot, filePath);
  }

  async readDirectory(folderPath) {
    return await fileOps.readDirectory(folderPath);
  }

  async loadRootDirectory() {
    const result = await this.readDirectory(this.projectRoot);

    const rawData = result.map((item) => ({
      ...item,
      pathInProject: `${this.projectName}/${item.name}`,
    }));
    return {
      name: this.projectRoot,
      root: true,
      key: this.projectRoot,
      title: this.projectRoot,
      path: this.projectRoot,
      pathInProject: this.projectRoot,
      loading: false,
      children: sortFile(rawData),
    };
  }

  async loadProjectFileTree() {
    const rootDirectory = await this.loadRootDirectory();

    for (let i = 0; i < rootDirectory.children.length; i++) {
      if (rootDirectory.children[i].type === "folder") {
        rootDirectory.children[i].children = await this.loadDirectoryRecursively(
          rootDirectory.children[i]
        );
      }
    }

    return rootDirectory;
  }

  async loadDirectoryRecursively(node) {
    const children = await this.loadDirectory(node);

    for (let i = 0; i < children.length; i++) {
      if (children[i].type === "folder") {
        children[i].children = await this.loadDirectoryRecursively(children[i]);
      }
    }

    return children;
  }

  async loadDirectory(node) {
    const result = await this.readDirectory(node.path);
    const rawData = result.map((item) => ({
      ...item,
      pathInProject: this.pathInProject(item.path),
    }));
    return sortFile(rawData);
  }

  async readProjectSettings() {
    this.projectSettings = new LocalProjectManager.ProjectSettings(
      this,
      this.settingsFilePath,
      LocalProjectManager.channel
    );
    await this.projectSettings.readSettings();
    return this.projectSettings;
  }

  openProjectSettings() {
    this.project.openProjectSettings(this.settingsFilePath);
  }

  get mainFilePath() {
    if (this.projectSettings?.get("main")) {
      return this.pathForProjectFile(this.projectSettings.get("main"));
    }
    throw new Error("No main file in project settings");
  }

  async isMainValid() {
    try {
      return await fileOps.isFile(this.mainFilePath);
    } catch (e) {
      return false;
    }
  }

  async getMainContract() {
    const mainFilePath = this.mainFilePath;
    const mainFile = await fileOps.readFile(mainFilePath);
    const existingPaths = new Set([mainFilePath]);
    const resultMap = new Map();
    await this.getContractsRecursively(mainFilePath, mainFile, resultMap, existingPaths);
    return Array.from(resultMap);
  }

  async getContractsRecursively(path, content, resultMap, existingFiles) {
    resultMap.set(path, content);

    const includeImportRegexp = /^#[ \t]*(include|import)[ \t]*"[^"]*"[ \t]*/gm;

    const imports = [...content.matchAll(includeImportRegexp)];
    if (imports === null) {
      return;
    }

    const importPaths = imports
      .map((e) => e[0])
      .map((importString) => {
        const importPath = importString.match(/"[^"]*"/g);
        return importPath !== null && importPath.length > 0 ? importPath[0] : "";
      })
      .map((e) => e.replace(/"/g, ""))
      .filter((e) => e !== "");

    const dirPath = path.substring(0, path.lastIndexOf("/"));

    for (let i = 0; i < importPaths.length; i++) {
      const absImportPath = pathHelper.join(dirPath, importPaths[i]);
      const cycleCheck = existingFiles.has(absImportPath);
      if (cycleCheck) {
        throw new Error(`Cycle deps beween "${absImportPath}" and "${path}"`);
      }
      if ((await fileOps.exists(absImportPath)) && (await fileOps.isFile(absImportPath))) {
        const importFileContent = await fileOps.readFile(absImportPath);
        existingFiles.add(absImportPath);
        await this.getContractsRecursively(
          absImportPath,
          importFileContent,
          resultMap,
          existingFiles
        );
        existingFiles.delete(absImportPath);
      } else {
        throw new Error(`Import file "${importPaths[i]}" in "${path}" not found`);
      }
    }
  }

  async checkSettings() {
    if (!this.project || !this.projectRoot) {
      notification.error("No Project", "Please open a project first.");
      return;
    }

    return await this.projectSettings.readSettings();
  }

  async isFile(filePath) {
    return await fileOps.isFile(filePath);
  }

  async ensureFile(filePath) {
    return await fileOps.fs.ensureFile(filePath);
  }

  async readFile(filePath, cb) {
    return await fileOps.readFile(filePath, cb);
  }

  async saveFile(filePath, content) {
    await fileOps.writeFile(filePath, content);
  }

  async createNewFile(basePath, name) {
    const filePath = pathHelper.join(basePath, name);
    if (await fileOps.exists(filePath)) {
      throw new Error(`File <b>${filePath}</b> already exists.`);
    }

    try {
      await fileOps.writeFile(filePath, "");
    } catch (e) {
      throw new Error(`Fail to create the file <b>${JSON.stringify(e)}</b>.`);
    }

    await this.refreshDirectory({
      type: "newFile",
      basePath,
      name,
      path: filePath,
    });
    return filePath;
  }

  async writeDirectory(basePath, name) {
    const folderPath = pathHelper.join(basePath, name);
    if (await fileOps.exists(folderPath)) {
      throw new Error(`Folder <b>${folderPath}</b> already exists.`);
    }

    try {
      await fileOps.writeDirectory(folderPath);
    } catch (e) {
      console.log(JSON.stringify(e));
      throw new Error(`File <b>${JSON.stringify(e)}</b> already exists.`);
    }

    await this.refreshDirectory({
      type: "newDirectory",
      basePath,
      name,
      path: folderPath,
    });
  }

  async moveOps(from, to, type) {
    try {
      if (type === "file") {
        await fileOps.copyMoveFile(from, to, "move");
      }

      if (type === "folder") {
        await fileOps.copyMoveFolder(from, to, "move");
      }
    } catch (e) {
      notification.error(`Move ${type} error`, e.message);
      return;
    }

    await this.refreshDirectory({
      type: type === "file" ? "moveFile" : "moveDirectory",
      targetPath: from,
      dropPath: to,
    });
  }

  async copyOps(from, to, type) {
    try {
      if (type === "file") {
        await fileOps.copyMoveFile(from, to, "copy");
      }

      if (type === "folder") {
        await fileOps.copyMoveFolder(from, to, "copy");
      }
    } catch (e) {
      notification.error(`Copy ${type} error`, e.message);
      return;
    }

    await this.refreshDirectory({
      type: type === "file" ? "copyFile" : "copyDirectory",
      targetPath: from,
      dropPath: to,
    });
  }

  async rename(oldPath, name) {
    const path = pathHelper;
    const { dir } = path.parse(oldPath);
    const newPath = path.join(dir, name);
    const isFile = await fileOps.isFile(oldPath);

    try {
      await fileOps.rename(oldPath, newPath);
      modelSessionManager.updateEditorAfterMovedFile(oldPath, newPath);
    } catch (e) {
      console.log(e);
      throw new Error(`Fail to rename <b>${oldPath}</b>.`);
    }

    await this.refreshDirectory({
      type: isFile ? "renameFile" : "renameDirectory",
      oldPath,
      newName: name,
      newPath,
    });
  }

  async deleteFile(node) {
    const { response } = await fileOps.showMessageBox({
      message: `Are you sure you want to delete ${node.path}?`,
      buttons: ["Move to Trash", "Cancel"],
    });
    if (response === 0) {
      if (node.children) {
        await fileOps.deleteDirectory(node.path);
      } else {
        await fileOps.deleteFile(node.path);
      }
      await this.refreshDirectory({
        type: node.children ? "deleteDirectory" : "deleteFile",
        path: node.path,
      });
    }
  }

  async refreshDirectory(data) {
    LocalProjectManager.channel.trigger("refresh-directory", data);
  }

  toggleTerminal(terminal) {
    LocalProjectManager.terminalButton?.setState({ terminal });
    this.project.toggleTerminal(terminal);
  }
}
