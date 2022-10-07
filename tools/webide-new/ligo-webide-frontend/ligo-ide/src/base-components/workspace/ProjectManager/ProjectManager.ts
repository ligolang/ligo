import pathHelper from "path-browserify";
import moment from "moment";
import * as monaco from "monaco-editor";
import fileOps, { FileInfo, FolderInfo } from "~/base-components/file-ops";
import notification from "~/base-components/notification";
import { modelSessionManager } from "~/base-components/code-editor";
import { IpcChannel } from "~/base-components/ipc";

import { sortFile } from "./helper";
import { getExamples } from "./examples";

import redux from "~/base-components/redux";

import { networkManager } from "~/ligo-components/eth-network";
import compilerManager, { CompilerManager } from "~/ligo-components/eth-compiler";
import queue from "~/ligo-components/eth-queue";

import ProjectSettings from "../ProjectSettings";

import type { WorkspaceLoader } from "../WorkspaceLoader";
import type TerminalButton from "../components/TerminalButton";
import { RefreshData } from "~base-components/filetree/types";
import { GistContent } from "~base-components/file-ops/GistFs";
import MonacoEditor from "~base-components/code-editor/MonacoEditor/MonacoEditor";

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

  // TODO: using Function as a type is not a great solution.
  // Each event has its own callback which should be possible to type.
  // Right now it is not really understandable what events we need
  // so in future it should be possible to type callbacks and remove eslint-disable.
  // eslint-disable-next-line @typescript-eslint/ban-types
  static effect(evt: string, callback: Function) {
    return () => {
      const dispose = ProjectManager.channel.on(evt, callback);
      ProjectManager.channel.trigger("current-value", evt);
      return dispose;
    };
  }

  static async createProject(name: string, template: string) {
    return this.processProject(name, undefined, template);
  }

  static async openProject(obj: GistContent, gistId: string, name?: string) {
    const projectData = obj;

    const projectsNames = await fileOps.getProjectNames();

    /* eslint-disable */
    const config = JSON.parse(projectData["/config.json"].content || "{}");
    config.gistId = gistId;

    const projectNameFromParams = name || (config.projectName ? config.projectName : gistId);
    let projectName: string = projectNameFromParams;

    if (!projectsNames.includes(projectNameFromParams)) {
      projectName = projectNameFromParams;
    } else {
      const getProjectNameRegex = new RegExp(/^.*(?=\((0|[1-9]{1}[0-9]*)\)$)/g);
      const isNumericProjectName = projectNameFromParams.match(getProjectNameRegex) !== null;
      const baseProjectName = isNumericProjectName
        ? projectNameFromParams.match(getProjectNameRegex)[0]
        : projectNameFromParams;

      const getNumberRegex = new RegExp(
        `(?<=^${baseProjectName}\\()(0|[1-9]{1}[0-9]*)(?=\\)$)`,
        "g"
      );
      const sameProjectNumbers = projectsNames
        .filter((pn) => pn.match(getNumberRegex) !== null)
        .map((pn) => Number(pn.match(getNumberRegex)));

      if (sameProjectNumbers.length === 0) {
        projectName = `${baseProjectName}(1)`;
      } else {
        projectName = `${baseProjectName}(${Math.max(...sameProjectNumbers) + 1})`;
      }
    }

    config.projectName = projectName;
    projectData["/config.json"].content = JSON.stringify(config);
    /* eslint-enable */

    return this.processProject(projectName, projectData, undefined);
  }

  static async processProject(
    name: string,
    obj: GistContent | undefined,
    template: string | undefined
  ) {
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
      const examples = getExamples(data.name, template, data.name);

      for (const file of Object.keys(examples)) {
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
    return Array.from(resultMap);
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
        await fileOps.copyMoveFolder(from, to, "copy");
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
      await fileOps.rename(oldPath, newPath);
      modelSessionManager.updateEditorAfterMovedFile(oldPath, newPath);
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
        await fileOps.deleteDirectory(node.path);
      } else {
        await fileOps.deleteFile(node.path);
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
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_S, () => {
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
    /* eslint-disable */
    // @ts-ignore
    if (CompilerManager.button.state.building) {
      /* eslint-enable */
      notification.error("Build Failed", "Another build task is running now.");
      return false;
    }

    const settings = await this.checkSettings();

    await this.project.saveAll();
    this.toggleTerminal(true);

    try {
      await compilerManager.build(settings, this, sourceFile);
    } catch {
      if (finalCall) {
        finalCall();
      }
      return false;
    }
    // if (result?.decorations) {
    //   modelSessionManager.updateDecorations(result.decorations);
    // }
    // if (result?.errors) {
    //   if (finalCall) {
    //     finalCall();
    //   }
    //   return false;
    // }

    // if (finalCall) {
    //   finalCall();
    // }
    return true;
  }

  // TODO: a great part of code was disabled for eslint, filled with
  // all ts autofixes and ts-ignore flags. The problem is that we do not
  // use this code right now and moreover if one day we will return to it
  // it will be rewrited. So, right now there is no reason to type and
  // pretty it.
  /* eslint-disable */
  async deploy(contractFileNode: { pathInProject: string; path: string }) {
    if (!networkManager.sdk) {
      notification.error(
        "Cannot Deploy",
        "No connected network. Please start a local network or switch to a remote network."
      );
      return;
    }

    let contracts;
    if (contractFileNode) {
      contractFileNode.pathInProject = this.pathInProject(contractFileNode.path);
      contracts = [contractFileNode];
    } else {
      try {
        contracts = await this.getMainContract(); // TODO this is not real getMainContract for this function as deploy is not implemented yet (old version of deploy function is used here)
      } catch {
        notification.error(
          "Cannot Deploy",
          "Cannot locate the built folder. Please make sure you have built the project successfully."
        );
        return;
      }
    }

    if (!contracts.length) {
      notification.error(
        "Cannot Deploy",
        "No built contracts found. Please make sure you have built the project successfully."
      );
      return;
    }

    this.deployButton.getDeploymentParameters(
      {
        contractFileNode: contractFileNode || (await this.getDefaultContractFileNode()),
        contracts,
      },
      (contractObj: any, allParameters: any) => this.pushDeployment(contractObj, allParameters),
      (contractObj: any, allParameters: any) => this.estimate(contractObj, allParameters)
    );
  }

  async getDefaultContractFileNode() {
    const settings = await this.checkSettings();
    // @ts-ignore
    if (!settings?.deploy) {
      return;
    }
    // @ts-ignore
    const filePath = this.pathForProjectFile(settings.deploy);
    const pathInProject = this.pathInProject(filePath);
    return { path: filePath, pathInProject };
  }

  async readProjectAbis() {
    const contracts = await this.getMainContract(); // TODO this is not real getMainContract for this function as readProjectAbis is not implemented yet (old version of readProjectAbis function is used here)
    const abis = await Promise.all(
      contracts.map((contract) =>
        fileOps
          // @ts-ignore
          .readFile(contract.path)
          .then((content) => ({
            // @ts-ignore
            contractPath: contract.path,
            // @ts-ignore
            pathInProject: this.pathInProject(contract.path),
            content: JSON.parse(content),
          }))
          .catch(() => null)
      )
    );
    // @ts-ignore
    return abis.filter(Boolean).map(({ contractPath, pathInProject, content }) => {
      const name = content.contractName || pathHelper.parse(contractPath).name;
      return {
        contractPath,
        pathInProject,
        name,
        abi: content?.abi,
        content,
      };
    });
  }

  checkSdkAndSigner(allParameters: { signer: any }) {
    if (!networkManager.sdk) {
      notification.error(
        "No Network",
        "No connected network. Please start a local network or switch to a remote network."
      );
      return true;
    }

    if (!allParameters.signer) {
      notification.error(
        "Deployment Error",
        "No signer specified. Please select one to sign the deployment transaction."
      );
      return true;
    }

    return false;
  }

  validateDeployment(contractObj: {
    bytecode: any;
    evm: { bytecode: { object: any }; deployedBytecode: { object: any } };
    deployedBytecode: any;
    abi: any;
  }) {
    let bytecode = contractObj.bytecode || contractObj.evm?.bytecode?.object;
    let deployedBytecode =
      contractObj.deployedBytecode || contractObj.evm?.deployedBytecode?.object;

    if (!deployedBytecode) {
      notification.error(
        "Deployment Error",
        "Invalid <b>deployedBytecode</b> and <b>evm.deployedBytecode.object</b> fields in the built contract JSON. Please make sure you selected a correct built contract JSON file."
      );
      return;
    }
    if (!deployedBytecode) {
      notification.error(
        "Deployment Error",
        "Invalid <b>bytecode</b> and <b>evm.bytecode.object</b> fields in the built contract JSON. Please make sure you selected a correct built contract JSON file."
      );
      return;
    }
    if (!bytecode.startsWith("0x")) {
      bytecode = `0x${bytecode}`;
    }
    if (!deployedBytecode.startsWith("0x")) {
      deployedBytecode = `0x${deployedBytecode}`;
    }
    return {
      abi: contractObj.abi,
      bytecode,
      deployedBytecode,
    };
  }

  async estimate(
    contractObj: {
      bytecode: any;
      evm: { bytecode: { object: any }; deployedBytecode: { object: any } };
      deployedBytecode: any;
      abi: any;
    },
    allParameters: { signer?: any; amount?: any; parameters?: any }
  ) {
    // @ts-ignore
    if (this.checkSdkAndSigner(allParameters)) {
      return;
    }
    const deploy = this.validateDeployment(contractObj);
    if (!deploy) {
      return;
    }

    const { amount, parameters } = allParameters;

    this.deployButton.setState({ pending: "Estimating..." });

    let result;
    try {
      const tx = await networkManager.sdk.getDeployTransaction(
        {
          abi: deploy.abi,
          bytecode: deploy.bytecode,
          // @ts-ignore
          options: deploy.options,
          parameters: parameters.array,
          amount,
        },
        {
          from: allParameters.signer,
        }
      );
      result = await networkManager.sdk.estimate(tx);
    } catch (e) {
      console.warn(e);
      // @ts-ignore
      notification.error("Estimate Failed", e.reason || e.message);
      this.deployButton.setState({ pending: false });
      return;
    }

    this.deployButton.setState({ pending: false });

    return result;
  }

  async pushDeployment(
    contractObj: {
      bytecode: any;
      evm: { bytecode: { object: any }; deployedBytecode: { object: any } };
      deployedBytecode: any;
      abi: any;
    },
    allParameters: {
      [x: string]: any;
      signer?: any;
      contractName?: any;
      amount?: any;
      parameters?: any;
    }
  ) {
    // @ts-ignore
    if (this.checkSdkAndSigner(allParameters)) {
      return;
    }
    const deploy = this.validateDeployment(contractObj);
    if (!deploy) {
      return;
    }

    this.deployButton.setState({ pending: "Deploying...", result: "" });

    const { networkId } = networkManager.sdk;
    const { contractName, amount, parameters, ...override } = allParameters;
    const codeHash = networkManager.sdk.utils.sign.sha3(deploy.deployedBytecode);

    let result;
    try {
      const tx = await networkManager.sdk.getDeployTransaction(
        {
          abi: deploy.abi,
          bytecode: deploy.bytecode,
          // @ts-ignore
          options: deploy.options,
          parameters: parameters.array,
          amount,
        },
        {
          from: allParameters.signer,
          ...override,
        }
      );

      result = await new Promise((resolve, reject) => {
        queue
          .add(
            () => networkManager.sdk.sendTransaction(tx),
            {
              title: "Deploy a Contract",
              name: "Deploy",
              contractName,
              signer: allParameters.signer,
              abi: deploy.abi,
              value: networkManager.sdk.utils.unit.toValue(amount || "0"),
              params: parameters.obj,
              ...override,
              modalWhenExecuted: true,
            },
            {
              pushing: () => this.deployButton.closeModal(),
              // @ts-ignore
              executed: ({ tx, receipt, abi }) => {
                resolve({
                  network: networkId,
                  codeHash,
                  ...parameters,
                  tx,
                  receipt,
                  abi,
                });
                return true;
              },
              "failed-timeout": reject,
              failed: reject,
            }
          )
          .catch(reject);
      });
    } catch (e) {
      console.warn(e);
      // @ts-ignore
      notification.error("Deploy Failed", e.reason || e.message);
      this.deployButton.setState({ pending: false });
      return;
    }

    this.deployButton.setState({ pending: false });
    notification.success("Deploy Successful");

    redux.dispatch("ABI_ADD", {
      // @ts-ignore
      ...deploy.options,
      name: contractName,
      // @ts-ignore
      codeHash: result.codeHash,
      abi: JSON.stringify(deploy.abi),
    });

    const deployResultPath = pathHelper.join(
      this.projectRoot,
      "deploys",
      // @ts-ignore
      `${result.network}_${moment().format("YYYYMMDD_HHmmss")}.json`
    );
    // @ts-ignore
    await this.writeFile(deployResultPath, JSON.stringify(result, null, 2));
  }
  /* eslint-enable */
}
