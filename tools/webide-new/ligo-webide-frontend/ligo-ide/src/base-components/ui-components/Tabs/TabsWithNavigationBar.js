import React, { PureComponent } from "react";

import Tabs from "./Tabs";
import NavigationBar from "./NavigationBar";

export default class TabsWithNavigationBar extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      noTab: false,
      tab: this.props.initialSelected,
      starredValues: new Set(this.props.starred),
    };
    this.tabs = React.createRef();
    this.navbar = React.createRef();
    this.tabIndex = this.props.initialTabs.length;
  }

  openTab = value => {
    const tabRef = this.tabs.current;
    if (!tabRef) {
      return;
    }
    const tab = tabRef.allTabs.find(item => item.value === value);
    if (tab) {
      tabRef.currentTab = tab;
    } else {
      tabRef.currentTab = this.createNewTab(value);
    }
  };

  onSelectTab = (tab = {}) => {
    this.setState({ tab }, () => {
      if (this.props.onChangeTab) {
        this.props.onChangeTab(tab.value || "");
      }
    });
  };

  onCloseTab = () => {
    this.navbar.current.recoverSelectionCache();
  };

  createNewTab = (value = "") => {
    this.tabIndex++;
    return { key: `tab-${this.tabIndex}`, value };
  };

  updateTab = updates => {
    this.tabs.current.updateTab(updates);
    if (updates.value) {
      this.navbar.current.setState({ value: updates.value });
    }
  };

  onToggleStar = (value, starred) => {
    if (starred) {
      this.state.starredValues.add(value);
    } else {
      this.state.starredValues.delete(value);
    }
    this.forceUpdate();
    if (this.props.onChangeStarred) {
      this.props.onChangeStarred(Array.from(this.state.starredValues));
    }
  };

  getTabText = ({ text, value, temp }) => {
    if (!this.props.getTabText) {
      return text;
    }
    if (!value) {
      return "New Tab";
    }
    return this.props.getTabText({ text, value, temp });
  };

  onTabsUpdated = tabs => {
    const tabValues = tabs.map(({ value }) => value);
    this.setState({ noTab: !tabs.length });
    this.props.onTabsUpdated(tabValues);
  };

  render() {
    const { noTab, tab } = this.state;

    const {
      initialTabs,
      onRefresh,
      NavbarButtons = null,
      tabContextMenu = [],
      children,
    } = this.props;

    return (
      <Tabs
        ref={this.tabs}
        size="sm"
        initialSelected={tab}
        initialTabs={initialTabs}
        tabContextMenu={tabContextMenu}
        getTabText={this.getTabText}
        onSelectTab={this.onSelectTab}
        createNewTab={this.createNewTab}
        tryCloseTab={() => this.onCloseTab}
        onTabsUpdated={this.onTabsUpdated}
        Bar={
          <NavigationBar
            ref={this.navbar}
            tab={tab}
            disabled={noTab}
            starred={this.state.starredValues.has(tab?.value)}
            onEnter={this.props.onValue}
            onRefresh={onRefresh}
            updateTab={this.updateTab}
            onToggleStar={this.onToggleStar}
          >
            {NavbarButtons}
          </NavigationBar>
        }
      >
        {children}
      </Tabs>
    );
  }
}
