import pathHelper from "path-browserify";
import debounce from "lodash/debounce";
import moment from "moment";
import * as monaco from "monaco-editor";
import fileOps from "~/base-components/file-ops";
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

export default class ProjectManager {
  static ProjectSettings = ProjectSettings;

  static channel = new IpcChannel("project");

  static terminalButton = null;

  static instance = null;

  constructor(project, projectRoot) {
    ProjectManager.instance = this;
    this.project = project;
    this.projectRoot = projectRoot;

    this.deployButton = null;
    this.onFileChanged = debounce(this.onFileChanged, 1500).bind(this);
  }

  onRefreshDirectory(callback) {
    ProjectManager.channel.on("refresh-directory", callback);
  }

  static effect(evt, callback) {
    return () => {
      const dispose = ProjectManager.channel.on(evt, callback);
      ProjectManager.channel.trigger("current-value", evt);
      return dispose;
    };
  }

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

  async loadRootDirectory() {
    const result = await fileOps.readDirectory(this.projectRoot);

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
    ProjectManager.channel.trigger("refresh-directory", data);
  }

  toggleTerminal(terminal) {
    ProjectManager.terminalButton?.setState({ terminal });
    this.project.toggleTerminal(terminal);
  }

  get settingsFilePath() {
    return this.pathForProjectFile("config.json");
  }

  onEditorReady(editor, editorComponent) {
    modelSessionManager.decorationMap = {};
    // eslint-disable-next-line no-bitwise
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_S, () => {
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

  lint() {}

  async compile(sourceFile, finalCall) {
    if (CompilerManager.button.state.building) {
      notification.error("Build Failed", "Another build task is running now.");
      return false;
    }

    const settings = await this.checkSettings();

    await this.project.saveAll();
    this.toggleTerminal(true);

    let result;
    try {
      result = await compilerManager.build(settings, this, sourceFile);
    } catch {
      finalCall && finalCall();
      return false;
    }
    if (result?.decorations) {
      modelSessionManager.updateDecorations(result.decorations);
    }
    if (result?.errors) {
      finalCall && finalCall();
      return false;
    }

    finalCall && finalCall();
    return true;
  }

  async deploy(contractFileNode) {
    if (!networkManager.sdk) {
      notification.error(
        "Cannot Deploy",
        "No connected network. Please start a local network or switch to a remote network."
      );
      return true;
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
      (contractObj, allParameters) => this.pushDeployment(contractObj, allParameters),
      (contractObj, allParameters) => this.estimate(contractObj, allParameters)
    );
  }

  async getDefaultContractFileNode() {
    const settings = await this.checkSettings();
    if (!settings?.deploy) {
      return;
    }
    const filePath = this.pathForProjectFile(settings.deploy);
    const pathInProject = this.pathInProject(filePath);
    return { path: filePath, pathInProject };
  }

  async readProjectAbis() {
    const contracts = await this.getMainContract(); // TODO this is not real getMainContract for this function as readProjectAbis is not implemented yet (old version of readProjectAbis function is used here)
    const abis = await Promise.all(
      contracts.map((contract) =>
        fileOps
          .readFile(contract.path)
          .then((content) => ({
            contractPath: contract.path,
            pathInProject: this.pathInProject(contract.path),
            content: JSON.parse(content),
          }))
          .catch(() => null)
      )
    );
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

  checkSdkAndSigner(allParameters) {
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
  }

  validateDeployment(contractObj) {
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

  async estimate(contractObj, allParameters) {
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
      notification.error("Estimate Failed", e.reason || e.message);
      this.deployButton.setState({ pending: false });
      return;
    }

    this.deployButton.setState({ pending: false });

    return result;
  }

  async pushDeployment(contractObj, allParameters) {
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
      notification.error("Deploy Failed", e.reason || e.message);
      this.deployButton.setState({ pending: false });
      return;
    }

    this.deployButton.setState({ pending: false });
    notification.success("Deploy Successful");

    redux.dispatch("ABI_ADD", {
      ...deploy.options,
      name: contractName,
      codeHash: result.codeHash,
      abi: JSON.stringify(deploy.abi),
    });

    const deployResultPath = pathHelper.join(
      this.projectRoot,
      "deploys",
      `${result.network}_${moment().format("YYYYMMDD_HHmmss")}.json`
    );
    await this.writeFile(deployResultPath, JSON.stringify(result, null, 2));
  }
}
