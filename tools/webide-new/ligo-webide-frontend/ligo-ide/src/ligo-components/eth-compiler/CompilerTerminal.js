import React, { PureComponent } from "react";

import { Tabs, TabContent, TabPane } from "~/base-components/ui-components";

import Terminal from "~/base-components/terminal";

import { CompilerManager } from "./compilerManager";
import TruffleTerminal from "./TruffleTerminal";

export default class CompilerTerminal extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      activeTab: "terminal",
    };

    CompilerManager.switchCompilerConsole = this.switchCompilerConsole.bind(this);
    this.tabs = React.createRef();

    this.initialTabs = [];
    if (props.projectManager.remote) {
      this.initialTabs.push({
        key: "terminal",
        text: (
          <span key="compiler-terminal">
            <i className="fas fa-hammer mr-1" />
            Compiler
          </span>
        ),
        clickCallback: () => {
          CompilerManager.terminal.inputRef.current.focus();
        },
      });
    } else {
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
      if (process.env.PROJECT === "eth") {
        // this.initialTabs.push({ key: 'truffle', text: <span key='compiler-truffle'><i className='fas fa-cookie mr-1' />Truffle</span> })
      }
    }
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
          <TabPane className="h-100 w-100" tabId="truffle">
            <TruffleTerminal active={active && activeTab === "truffle"} cwd={cwd} />
          </TabPane>
        </TabContent>
      </Tabs>
    );
  }
}
