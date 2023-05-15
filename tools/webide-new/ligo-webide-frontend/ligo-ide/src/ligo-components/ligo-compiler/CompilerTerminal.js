import React, { PureComponent } from "react";

import { Tabs, TabContent, TabPane } from "~/base-components/ui-components";

import Terminal from "~/base-components/terminal";

import { CompilerManager } from "./compilerManager";

export default class CompilerTerminal extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      activeTab: "terminal",
    };

    CompilerManager.switchCompilerConsole = this.switchCompilerConsole.bind(this);
    this.tabs = React.createRef();

    this.initialTabs = [];

    this.initialTabs.push({
      key: "terminal",
      text: (
        <span key="compiler-terminal">
          <i className="fas fa-folder-open mr-1" />
          Project
        </span>
      ),
      clickCallback: () => {
        CompilerManager.terminal.inputRef.current.focus();
      },
    });
  }

  switchCompilerConsole = (key) => {
    const tab = this.initialTabs.find((tab) => tab.key === key);
    this.tabs.current.currentTab = tab;
  };

  clearTerminal = () => {
    CompilerManager.terminal.clearContent();
  };

  render() {
    const { projectManager, active, cwd } = this.props;
    const { activeTab } = this.state;

    return (
      <Tabs
        ref={this.tabs}
        size="sm"
        headerClassName="nav-tabs-dark-active"
        noCloseTab
        initialSelected="terminal"
        initialTabs={this.initialTabs}
        onSelectTab={(tab) => this.setState({ activeTab: tab.key })}
        ToolButtons={[
          {
            icon: "fas fa-trash-alt",
            tooltip: "Clear",
            onClick: this.clearTerminal,
          },
        ]}
      >
        <TabContent className="h-100 w-100" activeTab={activeTab}>
          <TabPane className="h-100 w-100" tabId="terminal">
            <Terminal
              ref={(ref) => (CompilerManager.terminal = ref)}
              active={active && activeTab === "terminal"}
              cwd={cwd}
              logId="compiler-terminal"
              input={!projectManager.remote}
            />
          </TabPane>
        </TabContent>
      </Tabs>
    );
  }
}
