import React, { PureComponent } from "react";
import PropTypes from "prop-types";
import classnames from "classnames";
import { Tabs } from "~/base-components/ui-components";
import fileOps from "~/base-components/file-ops";
import { ClipBoardService } from "~/base-components/filetree";

import MonacoEditorContainer from "./MonacoEditor/MonacoEditorContainer";
import modelSessionManager from "./MonacoEditor/modelSessionManager";

export default class CodeEditorCollection extends PureComponent {
  static propTypes = {
    initialTab: PropTypes.object.isRequired,
    onSelectTab: PropTypes.func.isRequired,
    readOnly: PropTypes.bool,
    theme: PropTypes.string,
    onChangeDecorations: PropTypes.func.isRequired,
  };

  constructor(props) {
    super(props);
    this.state = {
      selectedTab: props.initialTab,
    };
    modelSessionManager.editorContainer = this;
    this.tabs = React.createRef();
    this.editorContainer = React.createRef();
    this.tabContextMenu = [
      {
        text: "Close",
        onClick: this.closeCurrentFile,
      },
      {
        text: "Close Others",
        onClick: this.closeOtherFiles,
      },
      {
        text: "Close Saved",
        onClick: this.closeSaved,
      },
      {
        text: "Close All",
        onClick: this.closeAll,
      },
      null,
      {
        text: "Copy Path",
        onClick: this.copyPath,
      },
    ];
  }

  openTab = (tab) => {
    this.tabs.current.currentTab = tab;
  };

  onSelectTab = (tab = { path: "" }) => {
    this.setState({ selectedTab: tab });
    this.props.onSelectTab(tab);
  };

  setCurrentTabUnsaved = (unsaved) => {
    this.tabs.current.updateTab({ unsaved });
  };

  // NOTE: there is no pathInProject props return in the local project, while will return it in remote project
  // see: packages/workspace/src/ProjectManager/LocalProjectManager.js
  // see: packages/workspace/src/ProjectManager/RemoteProjectManager.js
  copyPath = ({ pathInProject, path }) => {
    const filePath = pathInProject || path;
    const clipboard = new ClipBoardService();
    clipboard.writeText(filePath);
  };

  allUnsavedFiles = () =>
    this.tabs.current.allTabs
      .filter(({ path, unsaved }) => path && unsaved)
      .map(({ path }) => path);

  tryCloseTab = async (closingTab) => {
    if (closingTab.unsaved) {
      const willClose = await this.promptSave(closingTab.path);
      if (!willClose) {
        return false;
      }
    }
    return (tab) => modelSessionManager.closeModelSession(tab.path);
  };

  closeCurrentFile = () => {
    const { onCloseTab } = this.tabs.current;
    onCloseTab(this.tabs.current);
  };

  // MARK: may can define a batch delete in the Tabs component
  closeOtherFiles = (currentTab) => {
    const { onCloseTab, allTabs } = this.tabs.current;
    const shouldCloseTabs = allTabs.filter((tab) => tab.key !== currentTab.key);

    shouldCloseTabs.forEach((tab) => {
      onCloseTab(tab);
    });
  };

  closeSaved = () => {
    const { onCloseTab, allTabs } = this.tabs.current;
    const shouldCloseTabs = allTabs.filter((tab) => !tab.unsaved);

    shouldCloseTabs.forEach((tab) => {
      onCloseTab(tab);
    });
  };

  saveFile = async (filePath) => modelSessionManager.saveFile(filePath);

  promptSave = async (filePath) => {
    let clicked = false;
    // if (_.platform.isWeb) {
    //   const filename = windowPath.parse(path).base
    //   clicked = await $.modals.open('confirmUnsave', {
    //     title: `Do you want to save the changes in ${filename}?`,
    //     text: `Your changes will be lost if you don't save them.`
    //   })
    //   if (clicked === 'DUN_SAVE') {
    //     return true
    //   } else if (clicked) {
    //     await $.project.save(path)
    //     return true
    //   }
    //   return false
    // } else {
    //   const { response } = await remote.dialog.showMessageBox({
    //     // title: `Do you want to save the changes you made for ${path}?`,
    //     message: 'Your changes will be lost if you close this item without saving.',
    //     buttons: ['Save', 'Cancel', `Don't Save`]
    //   })
    //   clicked = response
    // }

    const { response } = await fileOps.showMessageBox({
      // title: `Do you want to save the changes you made for ${path}?`,
      message: "Your changes will be lost if you close this item without saving.",
      buttons: ["Save", "Cancel", "Don't Save"],
    });
    clicked = response;

    if (clicked === 0) {
      await this.saveFile(filePath);
      return true;
    }
    if (clicked === 2) {
      return true;
    }
    return false;
  };

  closeAll = () => {
    this.tabs.current.allTabs = [];
    modelSessionManager.closeAllModelSessions();
  };

  fileSaving = (filePath) => this.tabs.current.updateTab({ saving: true }, filePath);

  fileSaved = async (filePath, { saveAsPath, unsaved = false } = {}) => {
    const updates = { unsaved, saving: false };
    if (saveAsPath) {
      await this.editorContainer.current?.renameFile(filePath, saveAsPath);
      updates.key = saveAsPath;
      updates.path = saveAsPath;
    }
    const newTab = this.tabs.current.updateTab(updates, filePath);
    if (saveAsPath) {
      this.onSelectTab(newTab);
    }
  };

  onCommand = (cmd) => {
    switch (cmd) {
      case "save":
        modelSessionManager.saveCurrentFile();
        return;
      case "redo":
        modelSessionManager.redo();
        return;
      case "undo":
        modelSessionManager.undo();
        return;
      case "delete":
        modelSessionManager.delete();
        return;
      case "selectAll":
        modelSessionManager.selectAll();
        return;
      case "close-current-tab":
        return;
      case "project-settings":
        return;
      case "next-tab":
        this.tabs.current.nextTab();
        return;
      case "prev-tab":
        this.tabs.current.prevTab();
    }
  };

  refresh() {
    this.editorContainer?.current.forceUpdate();
  }

  render() {
    const { theme, editorConfig, projectRoot, initialTab, readOnly } = this.props;

    modelSessionManager.tabsRef = this.tabs;
    modelSessionManager.editorRef = this.editorContainer;

    return (
      <div
        className={classnames("d-flex w-100 h-100 overflow-hidden", {
          bg2: this.tabs.current && this.tabs.current.state.tabs.length !== 1,
        })}
      >
        <Tabs
          ref={this.tabs}
          size="sm"
          headerClassName="nav-tabs-dark-active"
          initialSelected={initialTab}
          onSelectTab={this.onSelectTab}
          tryCloseTab={this.tryCloseTab}
          createNewTab={undefined}
          getTabText={(tab) => modelSessionManager.tabTitle(tab)}
          tabContextMenu={this.tabContextMenu}
        >
          <MonacoEditorContainer
            ref={this.editorContainer}
            theme={theme}
            editorConfig={editorConfig}
            path={this.state.selectedTab.path}
            remote={this.state.selectedTab.remote}
            mode={this.state.selectedTab.mode}
            readOnly={readOnly}
            onChange={this.setCurrentTabUnsaved}
            onCommand={this.onCommand}
            onChangeDecorations={this.props.onChangeDecorations}
          />
        </Tabs>
      </div>
    );
  }
}
