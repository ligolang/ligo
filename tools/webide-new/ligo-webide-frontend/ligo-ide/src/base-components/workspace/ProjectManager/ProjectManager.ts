import pathHelper from "path-browserify";
import * as monaco from "monaco-editor";
import fileOps, { FileInfo, FolderInfo } from "~/base-components/file-ops";
import notification from "~/base-components/notification";
import { modelSessionManager } from "~/base-components/code-editor";
import { IpcChannel } from "~/base-components/ipc";

import { sortFile, getProjectName, getProjectNumber } from "./helper";
import { getExamples } from "./examples";

import redux from "~/base-components/redux";

import compilerManager from "~/ligo-components/eth-compiler";

import ProjectSettings from "../ProjectSettings";

import type { WorkspaceLoader } from "../WorkspaceLoader";
import type TerminalButton from "../components/TerminalButton";
import { RefreshData } from "~/base-components/filetree/types";
import { GistContent } from "~/base-components/file-ops/GistFs";
import MonacoEditor from "~/base-components/code-editor/MonacoEditor/MonacoEditor";

export type RawGistProjectType = {
  type: "rawgist";
  obj: GistContent;
  gistId: string;
  name?: string;
};

export type ProcessGistProject = {
  type: "gist";
  name: string;
  obj: GistContent;
};

export type ProcessGitProject = {
  type: "git";
  name: string;
  gitLink: string;
  branch?: string;
  token?: string;
};

export type ProcessTemplateProject = {
  type: "template";
  name: string;
  template: string;
  syntax: string;
};

export type ProcessProjectType = ProcessGistProject | ProcessGitProject | ProcessTemplateProject;

export default class ProjectManager {
  static ProjectSettings = ProjectSettings;

  static channel = new IpcChannel("project");

  static terminalButton: TerminalButton | undefined = undefined;

  static instance: ProjectManager | undefined = undefined;

  project: WorkspaceLoader;

  projectRoot: string;

  deployButton: any;

  projectSettings: ProjectSettings | undefined;

  projectName: string | undefined;

  constructor(project: WorkspaceLoader, projectRoot: string) {
    ProjectManager.instance = this;
    this.project = project;
    this.projectRoot = projectRoot;

    this.deployButton = null;
  }

  static onRefreshDirectory(callback: (data: RefreshData) => void) {
    ProjectManager.channel.on("refresh-directory", callback);
  }

  // eslint-disable-next-line @typescript-eslint/ban-types
  static effect(evt: string, callback: Function) {
    return () => {
      const dispose = ProjectManager.channel.on(evt, callback);
      ProjectManager.channel.trigger("current-value", evt);
      return dispose;
    };
  }

  static async createProject(name: string, template: string, syntax: string, gitLink?: string) {
    if (gitLink) {
      return this.processProject({
        type: "git",
        name,
        gitLink: `https://github.com/ligolang/${gitLink}`,
      });
    }
    return this.processProject({ type: "template", name, template, syntax });
  }

  static async openProject(projectInfo: RawGistProjectType | ProcessGitProject) {
    if (projectInfo.type === "git") {
      return this.processProject(projectInfo);
    }

    const projectData = projectInfo.obj;

    const projectsNames = await fileOps.getProjectNames();

    /* eslint-disable */
    const config = JSON.parse(projectData["/config.json"].content || "{}");
    config.gistId = projectInfo.gistId;

    const projectNameFromParams = projectInfo.name || (config.projectName ? config.projectName : projectInfo.gistId);
    let projectName: string = projectNameFromParams;

    if (!projectsNames.includes(projectNameFromParams)) {
      projectName = projectNameFromParams;
    } else {
      const isNumericProjectName = getProjectName(projectNameFromParams) !== null;
      const baseProjectName = isNumericProjectName
        ? getProjectName(projectNameFromParams)
        : projectNameFromParams;

      const sameProjectNumbers = projectsNames
        .filter((pn) => getProjectNumber(baseProjectName, pn) !== null)
        .map((pn) => Number(getProjectNumber(baseProjectName, pn)));

      if (sameProjectNumbers.length === 0) {
        projectName = `${baseProjectName}(1)`;
      } else {
        projectName = `${baseProjectName}(${Math.max(...sameProjectNumbers) + 1})`;
      }
    }

    config.projectName = projectName;
    projectData["/config.json"].content = JSON.stringify(config);
    /* eslint-enable */

    return this.processProject({ type: "gist", name: projectName, obj: projectData });
  }

  static async processProject(projectData: ProcessProjectType) {
    const { name } = projectData;

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

    if (projectData.type === "gist") {
      for (const key of Object.keys(projectData.obj)) {
        try {
          await fileOps.writeFile(`${data.path}/${key}`, projectData.obj[key].content);
        } catch (error) {
          console.error(error);
        }
      }
    }

    if (projectData.type === "git") {
      await fileOps.cloneGitRepo(
        data.name,
        projectData.gitLink,
        projectData.branch,
        projectData.token
      );
    }

    if (projectData.type === "template") {
      const examples = getExamples(data.name, projectData.template, data.name, projectData.syntax);

      for (const file of Object.keys(examples)) {
        const fileObject = examples[file];
        try {
          if (fileObject) {
            await fileOps.writeFile(fileObject.name, fileObject.content);
          }
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

  pathForProjectFile(relativePath: string) {
    return this.projectRoot ? pathHelper.join(this.projectRoot, relativePath) : "";
  }

  pathInProject(filePath: string) {
    return pathHelper.relative(this.projectRoot, filePath);
  }

  async loadRootDirectory() {
    const result = await fileOps.readDirectory(this.projectRoot);

    const rawData: (FolderInfo | FileInfo)[] = result.map((item) => ({
      ...item,
      pathInProject: `${this.projectName ? this.projectName : "undefined"}/${item.name}`,
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
      const child = rootDirectory.children[i];
      if (child.type === "folder") {
        child.children = await this.loadDirectoryRecursively(child);
      }
    }

    return rootDirectory;
  }

  async loadDirectoryRecursively(node: FolderInfo) {
    const children = await this.loadDirectory(node);

    for (let i = 0; i < children.length; i++) {
      const child = children[i];
      if (child.type === "folder") {
        child.children = await this.loadDirectoryRecursively(child);
      }
    }

    return children;
  }

  async loadDirectory(node: FolderInfo) {
    const result = await fileOps.readDirectory(node.path);
    const rawData = result.map((item) => ({
      ...item,
      pathInProject: this.pathInProject(item.path),
    }));
    return sortFile(rawData);
  }

  static getFilesFromContent = (c: (FolderInfo | FileInfo)[]) => {
    const filePaths: string[] = [];
    c.forEach((v) => {
      if (v.type === "file") {
        filePaths.push(v.path);
      } else {
        filePaths.push(...this.getFilesFromContent(v.children));
      }
    });
    return filePaths;
  };

  static async loadDirectoryFilePathsRecursively(path: string) {
    const children = await this.loadDirectoryFilePaths(path);

    for (let i = 0; i < children.length; i++) {
      const child = children[i];
      if (child.type === "folder") {
        child.children = await this.loadDirectoryFilePathsRecursively(child.path);
      }
    }

    return children;
  }

  static async loadDirectoryFilePaths(path: string) {
    const result = await fileOps.readDirectory(path);
    const rawData = result.map((item) => ({
      ...item,
      pathInProject: "",
    }));
    return rawData;
  }

  async readProjectSettings() {
    this.projectSettings = new ProjectManager.ProjectSettings(
      this,
      this.settingsFilePath,
      ProjectManager.channel
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
    const { mainFilePath } = this;
    const mainFile = await fileOps.readFile(mainFilePath);
    const existingPaths = new Set<string>([mainFilePath]);
    const resultMap = new Map<string, string>();
    await this.getContractsRecursively(mainFilePath, mainFile, resultMap, existingPaths);
    return Array.from(resultMap).map(([filePath, source]: [string, string]) => {
      return {
        filePath,
        source,
      };
    });
  }

  async getContractsRecursively(
    path: string,
    content: string,
    resultMap: Map<string, string>,
    existingFiles: Set<string>
  ): Promise<void> {
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
      return undefined;
    }

    return this.projectSettings?.readSettings();
  }

  static async createNewFile(basePath: string, name: string) {
    const filePath = pathHelper.join(basePath, name);
    if (await fileOps.exists(filePath)) {
      throw new Error(`File <b>${filePath}</b> already exists.`);
    }

    try {
      await fileOps.writeFile(filePath, "");
    } catch (e) {
      throw new Error(`Fail to create the file <b>${JSON.stringify(e)}</b>.`);
    }

    ProjectManager.refreshDirectory({
      type: "newFile",
      basePath,
      name,
      path: filePath,
    });
    return filePath;
  }

  static async writeFileWithEditorUpdate(filePath: string, content: string) {
    if (!(await fileOps.exists(filePath))) {
      throw new Error(`File <b>${filePath}</b> is not exists exists.`);
    }

    try {
      await fileOps.writeFile(filePath, content);
      modelSessionManager.updateEditorAfterMovedFile(filePath, { path: filePath, content });
    } catch (e) {
      throw new Error(`Fail to write to the file: <b>${JSON.stringify(e)}</b>.`);
    }
  }

  static async renameProject(name: string, newName: string) {
    try {
      const config = await fileOps.readFile(`.workspaces/${name}/config.json`);
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
      const configJSON = JSON.parse(config || "{}");
      // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
      configJSON.projectName = newName;
      await fileOps.writeFile(`.workspaces/${name}/config.json`, JSON.stringify(configJSON));
      await fileOps.rename(`.workspaces/${name}`, `.workspaces/${newName}`);
    } catch (e: any) {
      if (e instanceof Error) {
        notification.error("Rename project error", e.message);
        return;
      }
      throw e;
    }
    redux.dispatch("RENAME_PROJECT", { id: name, newName });
    notification.info("Rename Project Successful", `Project "${name}" renamed to "${newName}"`);
  }

  static async writeDirectory(basePath: string, name: string) {
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

    ProjectManager.refreshDirectory({
      type: "newDirectory",
      basePath,
      name,
      path: folderPath,
    });
  }

  static async moveOps(from: string, to: string, type: "file" | "folder") {
    try {
      if (type === "file") {
        await fileOps.copyMoveFile(from, to, "move");
        modelSessionManager.updateEditorAfterMovedFile(from, to);
      }

      if (type === "folder") {
        await fileOps.copyMoveFolder(from, to, "move");
      }
    } catch (e: any) {
      if (e instanceof Error) {
        notification.error(`Move ${type} error`, e.message);
        return;
      }
      throw e;
    }

    ProjectManager.refreshDirectory({
      type: type === "file" ? "moveFile" : "moveDirectory",
      targetPath: from,
      dropPath: to,
    });
  }

  static async copyOps(from: string, to: string, type: "file" | "folder") {
    try {
      if (type === "file") {
        await fileOps.copyMoveFile(from, to, "copy");
      }

      if (type === "folder") {
        const dirContent = await this.loadDirectoryFilePathsRecursively(from);
        const dirFiles = this.getFilesFromContent(dirContent);

        await fileOps.copyMoveFolder(from, to, "copy");
        for (let i = 0; i < dirFiles.length; i++) {
          modelSessionManager.updateEditorAfterMovedFile(
            dirFiles[i],
            dirFiles[i].replace(from, to)
          );
        }
      }
    } catch (e: any) {
      if (e instanceof Error) {
        notification.error(`Copy ${type} error`, e.message);
        return;
      }
      throw e;
    }

    ProjectManager.refreshDirectory({
      type: type === "file" ? "copyFile" : "copyDirectory",
      targetPath: from,
      dropPath: to,
    });
  }

  static async rename(oldPath: string, name: string) {
    const path = pathHelper;
    const { dir } = path.parse(oldPath);
    const newPath = path.join(dir, name);
    const isFile = await fileOps.isFile(oldPath);

    try {
      if (isFile) {
        await fileOps.rename(oldPath, newPath);
        modelSessionManager.updateEditorAfterMovedFile(oldPath, newPath);
      } else {
        const dirContent = await this.loadDirectoryFilePathsRecursively(oldPath);
        const dirFiles = this.getFilesFromContent(dirContent);

        await fileOps.rename(oldPath, newPath);
        for (let i = 0; i < dirFiles.length; i++) {
          modelSessionManager.updateEditorAfterMovedFile(
            dirFiles[i],
            dirFiles[i].replace(oldPath, newPath)
          );
        }
      }
    } catch (e) {
      console.log(e);
      throw new Error(`Fail to rename <b>${oldPath}</b>.`);
    }

    ProjectManager.refreshDirectory({
      type: isFile ? "renameFile" : "renameDirectory",
      oldPath,
      newName: name,
      newPath,
    });
  }

  static async deleteFile(node: FolderInfo | FileInfo) {
    const { response } = fileOps.showMessageBox({
      message: `Are you sure you want to delete ${node.path}?`,
    });
    if (response === 0) {
      if (node.type === "folder" && node.children) {
        const dirContent = await this.loadDirectoryFilePathsRecursively(node.path);
        const dirFiles = this.getFilesFromContent(dirContent);

        await fileOps.deleteDirectory(node.path);
        for (let i = 0; i < dirFiles.length; i++) {
          modelSessionManager.updateEditorAfterMovedFile(dirFiles[i], undefined);
        }
      } else {
        await fileOps.deleteFile(node.path);
        modelSessionManager.updateEditorAfterMovedFile(node.path, undefined);
      }
      ProjectManager.refreshDirectory({
        type: node.type === "folder" ? "deleteDirectory" : "deleteFile",
        path: node.path,
      });
    }
  }

  static refreshDirectory(data: RefreshData) {
    ProjectManager.channel.trigger("refresh-directory", data);
  }

  toggleTerminal(terminal: boolean) {
    ProjectManager.terminalButton?.setState({ terminal });
    this.project.toggleTerminal(terminal);
  }

  get settingsFilePath() {
    return this.pathForProjectFile("config.json");
  }

  onEditorReady(editor: monaco.editor.IStandaloneCodeEditor, editorComponent: MonacoEditor) {
    modelSessionManager.decorationMap = {};
    // eslint-disable-next-line no-bitwise
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS, () => {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-call
      editorComponent.props.onCommand("save");
      this.lint();
    });
    editor.onDidChangeModel(() => setTimeout(() => this.lint(), 100));
    setTimeout(() => this.lint(), 100);
    // reset the editor model sessions
  }

  onFileChanged() {
    this.lint();
  }

  // eslint-disable-next-line class-methods-use-this
  lint() {}

  async compile(sourceFile?: string, finalCall?: () => void) {
    await this.project.saveAll();
    this.toggleTerminal(true);

    try {
      await compilerManager.build(this);
    } catch {
      if (finalCall) {
        finalCall();
      }
      return false;
    }
    return true;
  }
}
