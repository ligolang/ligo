import { DockerImageChannel } from "~/base-components/docker";
import notification from "~/base-components/notification";
import { modelSessionManager } from "~/base-components/code-editor";
import fileOps from "~/base-components/file-ops";

import SolcjsCompiler from "./SolcjsCompiler";
import soljsonReleases from "./soljsonReleases.json";

class SolcjsChannel extends DockerImageChannel {
  installed() {
    return true;
  }

  versions() {
    const versions = Object.entries(soljsonReleases).map(([Tag, Name]) => ({
      Tag,
      Name,
    }));
    const event = new CustomEvent("versions", { detail: versions });
    this.eventTarget.dispatchEvent(event);
    return versions;
  }
}

export class CompilerManager {
  static button = null;

  static terminal = null;

  static truffleTerminal = null;

  constructor() {
    this.truffle = new DockerImageChannel(process.env.DOCKER_IMAGE_COMPILER);
    this.solc = new SolcjsChannel();
    this.notification = null;
    this.solcjsCompiler = new SolcjsCompiler();
  }

  get projectRoot() {
    if (!CompilerManager.terminal) {
      throw new Error("CompilerTerminal is not instantiated.");
    }
    return CompilerManager.terminal.props.cwd;
  }

  focus() {
    if (CompilerManager.terminal) {
      CompilerManager.terminal.focus();
    }
  }

  async execute(cmd) {
    CompilerManager.switchCompilerConsole("terminal");
    return await CompilerManager.terminal?.exec(cmd);
  }

  async cacheSolcBin(url, version) {
    const cacheStorage = await window.caches.open("solcjs");
    try {
      if (await cacheStorage.match(url)) {
        return;
      }
    } catch {
      await cacheStorage.delete(url);
    }

    this.notification = notification.info(
      "Downloadin Solc Bin",
      `Downloading <b>${version}</b>...`,
      0
    );
    const request = new Request(url, { mode: "no-cors" });
    const response = await fetch(request);
    await cacheStorage.put(url, response);
    this.notification.dismiss();
  }

  async buildBySolcjs(projectManager) {
    if (!(await projectManager.isMainValid)) {
      notification.error("No Main File", "Please specify the main file in project settings.");
      throw new Error("No Main File.");
    }

    const solcVersion = projectManager.projectSettings.get("compilers.solc");
    const solcFileName = soljsonReleases[solcVersion];

    // TODO: use the production proxy temporally
    const solcUrl = `https://eth.ide.black/solc/${solcFileName}`;

    const evmVersion = projectManager.projectSettings.get("compilers.evmVersion");
    const optimizer = projectManager.projectSettings.get("compilers.optimizer");

    CompilerManager.button.setState({ building: true });

    this.notification = notification.info("Building Project", "Building...", 0);

    const mainFilePath = projectManager.projectSettings.get("main");

    let mainFileExtension;
    let mainFileContent;
    try {
      mainFileExtension = projectManager.mainFilePath.split(".").pop();
      mainFileContent = await projectManager.readFile(projectManager.mainFilePath);
    } catch (e) {
      throw new Error(`Cannot read the main file <b>${mainFilePath}</b>.`);
    }
    CompilerManager.terminal.writeCmdToTerminal(
      `ligo compile contract ${projectManager.mainFilePath}`
    );

    const request = new Request("/api/compile", {
      method: "POST",
      headers: {
        Accept: "application/json",
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        fileExtension: mainFileExtension,
        source: mainFileContent,
      }),
    });
    fetch(request)
      .then(response => response.json())
      .then(async data => {
        CompilerManager.terminal.writeToTerminal(data.replace(/\n/g, "\n\r"));

        const buildFolder = projectManager.projectRoot + "/build";

        // Write output to file
        if (!await fileOps.exists(buildFolder)) {
          await projectManager.writeDirectory(projectManager.projectRoot, "build");
        }

        const buildRelatedPath = projectManager.mainFilePath.replace(projectManager.projectRoot + "/", "");
        const buildRelatedFolders = buildRelatedPath.split("/").slice(0, -1);

        let curFolder = buildFolder;
        for (let i = 0; i < buildRelatedFolders.length; i++) {
          if (!await fileOps.exists(curFolder + "/" + buildRelatedFolders[i])) {
            await projectManager.writeDirectory(curFolder, buildRelatedFolders[i]);
            curFolder = curFolder + "/" + buildRelatedFolders[i];
          }
        }

        const buildPath = projectManager.projectRoot
              + "/build"
              + projectManager.mainFilePath.replace(projectManager.projectRoot, "");
        const amendedBuildPath = buildPath.replace(/\.[^/.]+$/, ".tz");
        const fileFolder = amendedBuildPath.substring(0, amendedBuildPath.lastIndexOf("/"));
        const fileName = amendedBuildPath.substring(amendedBuildPath.lastIndexOf("/") + 1, amendedBuildPath.length);

        if (!await fileOps.exists(amendedBuildPath)) {
          await projectManager.createNewFile(fileFolder, fileName)
          await projectManager.saveFile(amendedBuildPath, data)
        } else {
          await projectManager.saveFile(amendedBuildPath, data)
        }

        CompilerManager.terminal.writeToTerminal("\nwrote output to " + "amendedBuildPath" + "\n\r\n\r");
      })
      .catch(e => {
        throw new Error(`Cannot compile contract <b>${JSON.stringify(e)}</b>.`);
      });

    this.notification.dismiss();
    CompilerManager.button.setState({ building: false });
  }

  async build(settings, projectManager, sourceFile) {
    // if (projectManager.remote) {
    return this.buildBySolcjs(projectManager);
    // }
  }

  static async stop() {
    if (CompilerManager.terminal) {
      CompilerManager.terminal.execAsChildProcess("docker stop -t 1 truffle-compile");
      await CompilerManager.terminal.stop();
      // await CompilerManager.terminal.execAsChildProcess(`docker rm $(docker ps --filter status=exited --filter ancestor=ethereum/solc:${compilers.solc} -q)`)
    }
  }

  parseSolcJSBuild(error) {
    const { prefix: projectPrefix, userId, projectId } = modelSessionManager.projectManager;
    const [prefix] = error.formattedMessage.match(/(?<=:).+(?=:)/g);
    const filePath = error.sourceLocation.file;
    const [row, column] = prefix.split(":");
    const lines = error.formattedMessage.split("\n");
    const { length } = lines[lines.length - 1].trim();

    return {
      filePath: `${projectPrefix}/${userId}/${projectId}/${filePath.replace("./", "")}`,
      text: `[Solcjs Compiler]: ${error.message}`,
      row: Number(row),
      length,
      type: "error",
      column: Number(column),
      from: "compiler",
    };
  }

  parseBuildLogs(msg) {
    let index;
    index = msg.indexOf("Compiling your contracts...");
    if (index > -1) {
      msg = msg.substr(index + 30);
    }
    const lines = msg.split("\n");

    const errors = [];
    let decorations = [];
    let status = "";
    let currentBlock = "";
    lines
      .map(line => line.trim())
      .forEach(line => {
        if (!line) {
          if (status === "ERROR") {
            errors.push(currentBlock.trim());
          } else if (status === "DECORATION") {
            decorations.push(currentBlock.trim());
          }
          status = "";
          currentBlock = "";
        } else if (line.startsWith("Error: ") || status === "ERROR") {
          status = "ERROR";
          currentBlock += `${line}\n`;
        } else if (line.startsWith(",/")) {
          if (status === "DECORATION") {
            decorations.push(currentBlock.trim());
          }
          status === "DECORATION";
          currentBlock = `${line.substr(1)}\n`;
        } else if (line.startsWith("/") || status === "DECORATION") {
          status = "DECORATION";
          currentBlock += `${line}\n`;
        }
      });
    decorations = decorations.map(msg => {
      const lines = msg.split("\n");
      const [prefix, ...rest] = lines[0].split(": ");
      const [filePath, row, column] = prefix.split(":");
      const text = rest.join(": ").trim();
      let type = "error";
      if (text.startsWith("Warning: ")) {
        type = "warning";
      }
      if (row && column) {
        const { length } = lines[lines.length - 1].trim();
        return {
          filePath,
          type,
          row: Number(row),
          column: Number(column),
          length,
          text,
        };
      }
      return { filePath, type, text };
    });
    return { errors, decorations };
  }
}

export default new CompilerManager();
