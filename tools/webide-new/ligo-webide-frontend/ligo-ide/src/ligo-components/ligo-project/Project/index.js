import findIndex from "lodash/findIndex";
import pathHelper from "path-browserify";
import { WorkspaceLoader } from "~/base-components/workspace";
import fileOps from "~/base-components/file-ops";
import {
  useBuiltinCustomTabs,
  modelSessionManager,
  defaultModeDetector,
} from "~/base-components/code-editor";
import compilerManager, { CompilerTerminal } from "~/ligo-components/ligo-compiler";

import ProjectToolbar from "./ProjectToolbar";
import ProjectSettingsTab from "./ProjectSettingsTab";

import { addLigoLanguages } from "./languages/addLigoLanguages";

// eslint-disable-next-line react-hooks/rules-of-hooks
useBuiltinCustomTabs(["markdown"]);
modelSessionManager.registerCustomTab("settings", ProjectSettingsTab, "Project Settings");
modelSessionManager.registerModeDetector((filePath) => {
  const { prefix, userId, projectId, settingsFilePath } = modelSessionManager.projectManager;
  const { base } = pathHelper.parse(filePath);
  const settingFilePath = settingsFilePath;
  const isRoot = settingFilePath === filePath;

  if (base === "config.json" && isRoot) {
    return "settings";
  }
  if (base.endsWith(".mligo")) {
    return "cameligoext";
  }
  if (base.endsWith(".jsligo")) {
    return "jsligoext";
  }
  if (base.endsWith(".tz")) {
    return "tzext";
  }
  return defaultModeDetector(filePath);
});

const makeContextMenu = (contextMenu, projectManager) => (node) => {
  if (!node) {
    return [];
  }

  if (node.children) {
    const menus = [...contextMenu];
    const index = findIndex(contextMenu, (item) => item?.text === "Open");
    if (index !== -1) {
      menus.splice(index, 2);
    }
    return menus;
  }

  return contextMenu;
};

WorkspaceLoader.defaultProps = {
  compilerManager,
  ProjectToolbar,
  CompilerTerminal,
  addLanguages: addLigoLanguages,
  makeContextMenu,
};

export { WorkspaceLoader };
