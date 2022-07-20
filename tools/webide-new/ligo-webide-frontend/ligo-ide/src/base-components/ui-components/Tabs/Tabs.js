import React, { PureComponent } from "react";
import PropTypes from "prop-types";
import classnames from "classnames";
import find from "lodash/find";
import findIndex from "lodash/findIndex";
import cloneDeep from "lodash/cloneDeep";
import Screen from "../Screen/Screen";
import TabHeader from "./TabHeader";

export default class Tabs extends PureComponent {
  static propTypes = {
    size: PropTypes.string,
    headerClassName: PropTypes.string,
    initialTabs: PropTypes.array,
    tabContextMenu: PropTypes.array,
    initialSelected: PropTypes.oneOfType([PropTypes.string, PropTypes.object]).isRequired,
    onSelectTab: PropTypes.func,
    createNewTab: PropTypes.func,
    getTabText: PropTypes.func,
    tryCloseTab: PropTypes.func,
    onTabsUpdated: PropTypes.func,
  };

  static defaultProps = {
    onSelectTab: () => {},
  };

  constructor(props) {
    super(props);
    const { initialTabs, initialSelected } = this.props;

    const selected = this.tabObj(initialSelected);

    if (initialTabs) {
      const tabs = initialTabs.map(this.tabObj);
      if (!find(tabs, ["key", selected.key])) {
        tabs.push(selected);
      }
      this.state = { selected, tabs };
      return;
    }

    this.header = React.createRef();
    this.state = {
      selected,
      tabs: [selected],
    };
  }

  tabKey = (tab = this.state.selected) => (typeof tab === "object" ? tab.key : tab);

  tabObj = (tab = this.state.selected) => (typeof tab === "object" ? tab : { key: tab, text: tab });

  findTab = (key = this.tabKey()) => find(this.state.tabs, ["key", key]);

  findTabIndex = (key = this.tabKey()) => findIndex(this.state.tabs, ["key", key]);

  set currentTab(tab) {
    if (!tab) {
      this.props.onSelectTab();
      this.setState({ selected: [], tabs: [] });
      return;
    }

    const selected = this.tabObj(tab);
    if (this.tabKey() === selected.key) {
      return;
    }

    if (this.findTab(selected.key)) {
      this.setState({ selected });
    } else {
      const newState = {
        selected,
        tabs: [...this.state.tabs, selected],
      };
      this.setState(newState, this.onTabsUpdated);
    }
    this.props.onSelectTab(selected);
  }

  get currentTab() {
    return this.tabObj();
  }

  resetTabs = (selected, tabs) => {
    return this.setState({
      selected: this.tabObj(selected),
      tabs: [...tabs.map(this.tabObj)],
    });
  };

  set allTabs(tabs) {
    this.setState(
      {
        tabs: [...tabs.map(this.tabObj)],
        selected: tabs.length === 0 ? [] : this.state.selected,
      },
      this.onTabsUpdated
    );
  }

  get allTabs() {
    return [...this.state.tabs];
  }

  nextTab = () => {
    if (this.state.tabs.length < 2) {
      return;
    }
    const index = this.findTabIndex();
    const nTabs = this.state.tabs.length;
    this.currentTab = this.state.tabs[(index + 1) % nTabs];
  };

  prevTab = () => {
    if (this.state.tabs.length < 2) {
      return;
    }
    const index = this.findTabIndex();
    const nTabs = this.state.tabs.length;
    this.currentTab = this.state.tabs[(index + nTabs - 1) % nTabs];
  };

  updateTab = (updates, key = this.tabKey()) => {
    const target = this.findTab(key);
    const index = this.findTabIndex();

    if (target) {
      Object.keys(updates).forEach((key) => {
        target[key] = updates[key];
      });
      const tabs = cloneDeep(this.state.tabs);
      tabs.splice(index, 1, target);

      this.setState({
        tabs,
      });
      this.onTabsUpdated();
    }
    return target;
  };

  dragMoveTab(dragIndex, hoverIndex) {
    const tabs = cloneDeep(this.state.tabs);

    tabs[dragIndex] = tabs.splice(hoverIndex, 1, tabs[dragIndex])[0];
    this.setState({
      tabs,
    });
  }

  changeCurrentTab = (tab) => {
    const found = this.findTab(this.tabKey(tab));
    if (found) {
      this.currentTab = found;
      return;
    }
    this.props.onSelectTab(this.updateTab(this.tabObj(tab)));
  };

  // private
  onNewTab = async () => {
    try {
      this.currentTab = await this.props.createNewTab();
    } catch {}
  };

  onCloseTab = async (closingTab) => {
    let closeTabHandler;
    if (this.props.tryCloseTab) {
      closeTabHandler = await this.props.tryCloseTab(closingTab);
      if (!closeTabHandler) {
        return;
      }
    }

    // TODO: cannot close all tabs
    // if (this.state.tabs.length === 1) {
    //   return
    // }

    const index = this.findTabIndex(closingTab.key);
    if (index === -1) {
      return;
    }

    const newTabs = [...this.state.tabs];
    const isInTab = newTabs[0].key.indexOf("tab") !== -1;
    newTabs.splice(index, 1);

    if (closingTab.key === this.tabKey()) {
      if (newTabs.length > 0) {
        this.currentTab = newTabs[index ? index - 1 : 0];
      } else {
        this.currentTab = null;
      }
    }

    this.setState({ tabs: newTabs }, this.onTabsUpdated);
    newTabs.length === 0 && isInTab && (await this.onNewTab());
    closeTabHandler && closeTabHandler(closingTab);
  };

  onTabsUpdated = () => {
    if (this.props.onTabsUpdated) {
      this.props.onTabsUpdated(this.state.tabs);
    }
  };

  render() {
    const { Bar = null, children } = this.props;
    const { tabs } = this.state;
    let content;
    if (tabs.length) {
      content = children;
    } else {
      content = (
        <Screen>
          <h4 className="display-4">No Opened Tab</h4>
          <p />
        </Screen>
      );
    }

    return (
      <div
        className={classnames(
          "d-flex flex-column w-100 h-100 overflow-hidden",
          this.props.className
        )}
      >
        <TabHeader
          ref={this.header}
          size={this.props.size}
          className={this.props.headerClassName}
          tabs={this.state.tabs}
          selected={this.tabObj()}
          onSelectTab={(tab) => (this.currentTab = tab)}
          getTabText={this.props.getTabText}
          onNewTab={this.props.createNewTab && this.onNewTab}
          onCloseTab={this.props.noCloseTab ? undefined : this.onCloseTab}
          onDragTab={this.dragMoveTab.bind(this)}
          ToolButtons={this.props.ToolButtons}
          contextMenu={this.props.tabContextMenu}
        />
        <div className="d-flex flex-1 flex-column overflow-hidden p-relative">
          {Bar}
          {content}
        </div>
      </div>
    );
  }
}
