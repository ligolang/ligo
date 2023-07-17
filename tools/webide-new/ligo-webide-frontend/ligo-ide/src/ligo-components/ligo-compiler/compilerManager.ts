import notification from "~/base-components/notification";
import { Notification } from "~/base-components/notification/manager";
import fileOps from "~/base-components/file-ops";

import { WebIdeApi } from "~/components/api/api";
import ProjectManager from "~/base-components/workspace/ProjectManager/ProjectManager";
import Terminal from "~/base-components/terminal";

import redux from "~/base-components/redux";

export class CompilerManager {
  static terminal: Terminal | null = null;

  notification: Notification | null;

  constructor() {
    this.notification = null;
  }

  static focus() {
    if (CompilerManager.terminal) {
      CompilerManager.terminal.focus();
    }
  }

  async build(projectManager: ProjectManager) {
    if (!(await projectManager.isMainValid())) {
      notification.error(
        "No Main File",
        `Main file <b>${projectManager.mainFilePath}</b> from settings is not found.`
      );
      throw new Error("No Main File.");
    }

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
    const module: string = projectManager.projectSettings?.get("module") ?? "";
    const moduleArg = module !== "" ? `-m ${module}` : "";
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const { protocol } = redux.getState();
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access
    const name: string = protocol?.name ?? "";
    const protocolArg = name !== "" ? `--protocol ${name}` : "";
    if (CompilerManager.terminal) {
      CompilerManager.terminal.writeCmdToTerminal(
        `ligo compile contract ${projectManager.mainFilePath} ${protocolArg} ${moduleArg}`
      );
    }

    // UGLY FIX : pass module value only if it is non-empty
    const project =
      module !== ""
        ? {
            project: {
              sourceFiles: contractFiles,
              main: projectManager.mainFilePath,
              module,
            },
          }
        : {
            project: {
              sourceFiles: contractFiles,
              main: projectManager.mainFilePath,
            },
          };

    WebIdeApi.compileContract(project)
      .then(async (resp) => {
        if (CompilerManager.terminal) {
          CompilerManager.terminal.writeToTerminal(resp.data.replace(/\n/g, "\n\r"));
        }

        const amendedBuildPath = await CompilerManager.saveCompiledContract(
          resp.data,
          projectManager
        );

        if (CompilerManager.terminal) {
          if (amendedBuildPath.type === "success") {
            CompilerManager.terminal.writeToTerminal(
              `\nwrote output to ${amendedBuildPath.message}\n\r\n\r`
            );
          }
          if (amendedBuildPath.type === "error") {
            CompilerManager.terminal.writeToTerminal(
              `\n${amendedBuildPath.message}\n\r\n\r`,
              "red"
            );
          }
        }
      })
      .catch((e) => {
        if (e instanceof Error && CompilerManager.terminal) {
          const reg = /(\r\n?|\n|\t)/g;
          const ntext = e.message.replace(reg, "\r\n");
          CompilerManager.terminal.writeToTerminal(`\n${ntext}\n\r\n\r`);
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
      } else if (await fileOps.isFile(`${curFolder}/${buildRelatedFolders[i]}`)) {
        return {
          type: "error",
          message: `Error during contract saving: on path ${buildFolder}/${buildRelatedPath}, path ${curFolder}/${buildRelatedFolders[i]} is already existing file`,
        };
      }
      curFolder = `${curFolder}/${buildRelatedFolders[i]}`;
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
      await ProjectManager.writeFileWithEditorUpdate(amendedBuildPath, data);
    } else {
      await ProjectManager.writeFileWithEditorUpdate(amendedBuildPath, data);
    }

    return { type: "success", message: amendedBuildPath };
  }
}

export default new CompilerManager();
