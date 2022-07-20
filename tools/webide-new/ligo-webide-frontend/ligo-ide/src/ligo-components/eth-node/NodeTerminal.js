import React, { PureComponent } from "react";
import classnames from "classnames";

import { Tabs, TabContent, TabPane } from "~/base-components/ui-components";

import Terminal from "~/base-components/terminal";

import nodeManager from "./nodeManager";

// const parser = /block:\s(\d+),/
const parser = /Height=(\d+)/;
function parseLine(line) {
  const match = parser.exec(line);
  if (match && match[1]) {
    nodeManager.updateBlockNumber(match[1]);
  }
  return line;
}

let incompleteLine = "";
function onLogReceived(message) {
  message = incompleteLine + message;

  let lines = message.split("\n");
  incompleteLine = lines.pop();
  lines.push("");
  lines = lines.map(parseLine);
  return lines.join("\n");
}

export default class NodeTerminal extends PureComponent {
  constructor(props) {
    super(props);

    this.state = {
      activeTab: props.miner ? "miner" : "node",
    };
    this.tabs = React.createRef();
  }

  componentDidUpdate(prevProps) {
    if (this.props.miner === prevProps.miner) {
      return;
    }
    if (this.props.miner) {
      this.openMinerTab();
    } else {
      this.closeMinerTab();
    }
  }

  getTabs = ({ miner, indexer } = {}) => {
    const tabs = [this.defaultTab];
    if (miner) {
      tabs.push(this.tabFor("miner"));
    }
    if (indexer) {
      tabs.push(this.tabFor("indexer"));
    }
    return tabs;
  };

  get defaultTab() {
    return {
      key: "node",
      text: (
        <span key="terminal-node">
          <i className="fas fa-server mr-1" />
          node
        </span>
      ),
    };
  }

  tabFor = (type) => {
    const key = type;
    const text = type;
    let icon;
    if (type === "miner") {
      icon = "fas fa-hammer";
    } else if (type === "indexer") {
      icon = "fas fa-indent";
    }
    return {
      key,
      text: (
        <span key={`terminal-${key}`}>
          <i className={classnames(icon, "mr-1")} />
          {text}
        </span>
      ),
    };
  };

  openMinerTab = () => {
    this.tabs.current.setState({ tabs: this.getTabs({ miner: true }) });
    setTimeout(() => this.tabs.current.onCloseTab({ key: "node" }), 100);
  };

  closeMinerTab = () => {
    this.tabs.current.setState({ tabs: this.getTabs({ miner: true }) });
    setTimeout(() => this.tabs.current.onCloseTab({ key: "miner" }), 100);
  };

  clearTerminal = () => {
    switch (this.state.activeTab) {
      case "node":
        nodeManager._terminal?.clearContent();
        return;
      case "miner":
        nodeManager._minerTerminal?.clearContent();
        return;
      case "indexer":
        nodeManager._indexerTerminal?.clearContent();

      default:
    }
  };

  onNodeFinished = (result) => {
    nodeManager._nodeButton?.stop();
  };

  render() {
    const { active, miner, indexer } = this.props;
    const { activeTab } = this.state;
    const initialTabs = this.getTabs({ miner, indexer });

    return (
      <Tabs
        ref={this.tabs}
        headerClassName="nav-tabs-dark-active"
        noCloseTab
        initialSelected="node"
        initialTabs={initialTabs}
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
          <TabPane className="h-100 w-100" tabId="node">
            <Terminal
              logId="node-instance"
              active={active && activeTab === "node"}
              ref={(ref) => (nodeManager.terminal = ref)}
              onLogReceived={onLogReceived}
              onFinished={this.onNodeFinished}
            />
          </TabPane>
          <TabPane className="h-100 w-100" tabId="miner">
            <Terminal
              logId="node-miner"
              active={active && activeTab === "miner"}
              ref={(ref) => (nodeManager.minerTerminal = ref)}
              onLogReceived={onLogReceived}
            />
          </TabPane>
          <TabPane className="h-100 w-100" tabId="indexer">
            <Terminal
              logId="node-indexer"
              active={active && activeTab === "indexer"}
              ref={(ref) => (nodeManager.indexerTerminal = ref)}
              onLogReceived={onLogReceived}
            />
          </TabPane>
        </TabContent>
      </Tabs>
    );
  }
}
