import { DockerImageChannel } from "~/base-components/docker";
import notification from "~/base-components/notification";
import { Notification } from "~/base-components/notification/manager";
// import { modelSessionManager } from "~/base-components/code-editor";
import fileOps from "~/base-components/file-ops";

import SolcjsCompiler from "./SolcjsCompiler";
// import soljsonReleases from "./soljsonReleases.json";
import { WebIdeApi } from "~/components/api/api";
import ProjectManager from "~/base-components/workspace/ProjectManager/ProjectManager";
import Terminal from "~/base-components/terminal";

export class CompilerManager {
  static terminal: Terminal | null = null;

  // static truffleTerminal = null;

  truffle: DockerImageChannel;

  notification: Notification | null;

  solcjsCompiler: SolcjsCompiler;

  constructor() {
    this.truffle = new DockerImageChannel(process.env.DOCKER_IMAGE_COMPILER);
    this.notification = null;
    this.solcjsCompiler = new SolcjsCompiler();
  }

  // static get projectRoot() {
  //   if (!CompilerManager.terminal) {
  //     throw new Error("CompilerTerminal is not instantiated.");
  //   }
  //   return CompilerManager.terminal.props.cwd;
  // }

  static focus() {
    if (CompilerManager.terminal) {
      CompilerManager.terminal.focus();
    }
  }

  // async execute(cmd) {
  //   CompilerManager.switchCompilerConsole("terminal");
  //   return await CompilerManager.terminal?.exec(cmd);
  // }

  async cacheSolcBin(url: URL | RequestInfo, version: string) {
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

  async buildBySolcjs(projectManager: ProjectManager) {
    if (!(await projectManager.isMainValid())) {
      notification.error(
        "No Main File",
        `Main file <b>${projectManager.mainFilePath}</b> from settings is not found.`
      );
      throw new Error("No Main File.");
    }

    // const solcVersion = projectManager.projectSettings.get("compilers.solc");
    // const solcFileName = soljsonReleases[solcVersion];

    // TODO: use the production proxy temporally
    // const solcUrl = `https://eth.ide.black/solc/${solcFileName}`;

    // const evmVersion = projectManager.projectSettings.get("compilers.evmVersion");
    // const optimizer = projectManager.projectSettings.get("compilers.optimizer");

    this.notification = notification.info("Building Project", "Building...", 0);

    let contractFiles = [];
    try {
      contractFiles = await projectManager.getMainContract();
    } catch (e) {
      if (e instanceof Error) {
        notification.error("Compilation error", e.message);
      } else {
        console.error(JSON.stringify(e));
      }
      return;
    }

    if (CompilerManager.terminal) {
      CompilerManager.terminal.writeCmdToTerminal(
        `ligo compile contract ${projectManager.mainFilePath}`
      );
    }

    WebIdeApi.compileContract({
      project: {
        sourceFiles: contractFiles,
        main: projectManager.mainFilePath,
      },
    })
      .then(async (resp) => {
        if (CompilerManager.terminal) {
          CompilerManager.terminal.writeToTerminal(resp.data.replace(/\n/g, "\n\r"));
        }

        const amendedBuildPath = await CompilerManager.saveCompiledContract(
          resp.data,
          projectManager
        );

        if (CompilerManager.terminal) {
          CompilerManager.terminal.writeToTerminal(`\nwrote output to ${amendedBuildPath}\n\r\n\r`);
        }
      })
      .catch((e) => {
        if (e instanceof Error) {
          notification.error("Compilation error", e.message);
        } else {
          console.error(JSON.stringify(e));
        }
      });

    this.notification.dismiss();
  }

  static async saveCompiledContract(data: string, projectManager: ProjectManager) {
    const buildFolder = `${projectManager.projectRoot}/build`;

    // Write output to file
    if (!(await fileOps.exists(buildFolder))) {
      await ProjectManager.writeDirectory(projectManager.projectRoot, "build");
    }

    const buildRelatedPath = projectManager.mainFilePath.replace(
      `${projectManager.projectRoot}/`,
      ""
    );
    const buildRelatedFolders = buildRelatedPath.split("/").slice(0, -1);

    let curFolder = buildFolder;
    for (let i = 0; i < buildRelatedFolders.length; i++) {
      if (!(await fileOps.exists(`${curFolder}/${buildRelatedFolders[i]}`))) {
        await ProjectManager.writeDirectory(curFolder, buildRelatedFolders[i]);
        curFolder = `${curFolder}/${buildRelatedFolders[i]}`;
      }
    }

    const buildPath = `${projectManager.projectRoot}/build${projectManager.mainFilePath.replace(
      projectManager.projectRoot,
      ""
    )}`;
    const amendedBuildPath = buildPath.replace(/\.[^/.]+$/, ".tz");
    const fileFolder = amendedBuildPath.substring(0, amendedBuildPath.lastIndexOf("/"));
    const fileName = amendedBuildPath.substring(
      amendedBuildPath.lastIndexOf("/") + 1,
      amendedBuildPath.length
    );

    if (!(await fileOps.exists(amendedBuildPath))) {
      await ProjectManager.createNewFile(fileFolder, fileName);
      await fileOps.writeFile(amendedBuildPath, data);
    } else {
      await fileOps.writeFile(amendedBuildPath, data);
    }

    return amendedBuildPath;
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  async build(settings: any, projectManager: ProjectManager, sourceFile: any) {
    // if (projectManager.remote) {
    return this.buildBySolcjs(projectManager);
    // }
  }

  static async stop() {
    if (CompilerManager.terminal) {
      await CompilerManager.terminal.execAsChildProcess("docker stop -t 1 truffle-compile");
      await CompilerManager.terminal.stop();
      // await CompilerManager.terminal.execAsChildProcess(`docker rm $(docker ps --filter status=exited --filter ancestor=ethereum/solc:${compilers.solc} -q)`)
    }
  }

  // parseSolcJSBuild(error) {
  //   const { prefix: projectPrefix, userId, projectId } = modelSessionManager.projectManager;
  //   const [prefix] = error.formattedMessage.match(/(?<=:).+(?=:)/g);
  //   const filePath = error.sourceLocation.file;
  //   const [row, column] = prefix.split(":");
  //   const lines = error.formattedMessage.split("\n");
  //   const { length } = lines[lines.length - 1].trim();

  //   return {
  //     filePath: `${projectPrefix}/${userId}/${projectId}/${filePath.replace("./", "")}`,
  //     text: `[Solcjs Compiler]: ${error.message}`,
  //     row: Number(row),
  //     length,
  //     type: "error",
  //     column: Number(column),
  //     from: "compiler",
  //   };
  // }

  // parseBuildLogs(msg) {
  //   let index;
  //   index = msg.indexOf("Compiling your contracts...");
  //   if (index > -1) {
  //     msg = msg.substr(index + 30);
  //   }
  //   const lines = msg.split("\n");

  //   const errors = [];
  //   let decorations = [];
  //   let status = "";
  //   let currentBlock = "";
  //   lines
  //     .map((line) => line.trim())
  //     .forEach((line) => {
  //       if (!line) {
  //         if (status === "ERROR") {
  //           errors.push(currentBlock.trim());
  //         } else if (status === "DECORATION") {
  //           decorations.push(currentBlock.trim());
  //         }
  //         status = "";
  //         currentBlock = "";
  //       } else if (line.startsWith("Error: ") || status === "ERROR") {
  //         status = "ERROR";
  //         currentBlock += `${line}\n`;
  //       } else if (line.startsWith(",/")) {
  //         if (status === "DECORATION") {
  //           decorations.push(currentBlock.trim());
  //         }
  //         status === "DECORATION";
  //         currentBlock = `${line.substr(1)}\n`;
  //       } else if (line.startsWith("/") || status === "DECORATION") {
  //         status = "DECORATION";
  //         currentBlock += `${line}\n`;
  //       }
  //     });
  //   decorations = decorations.map((msg) => {
  //     const lines = msg.split("\n");
  //     const [prefix, ...rest] = lines[0].split(": ");
  //     const [filePath, row, column] = prefix.split(":");
  //     const text = rest.join(": ").trim();
  //     let type = "error";
  //     if (text.startsWith("Warning: ")) {
  //       type = "warning";
  //     }
  //     if (row && column) {
  //       const { length } = lines[lines.length - 1].trim();
  //       return {
  //         filePath,
  //         type,
  //         row: Number(row),
  //         column: Number(column),
  //         length,
  //         text,
  //       };
  //     }
  //     return { filePath, type, text };
  //   });
  //   return { errors, decorations };
  // }
}

export default new CompilerManager();
