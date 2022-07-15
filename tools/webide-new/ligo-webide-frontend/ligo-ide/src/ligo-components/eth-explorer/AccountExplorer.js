import React from "react";

import { withRouter } from "react-router-dom";
import { TabbedExplorer, Screen, Button } from "~/base-components/ui-components";

import redux, { connect } from "~/base-components/redux";
import keypairManager from "~/base-components/keypair";

import AccountPage from "./AccountPage";

import TransferButton from "./buttons/TransferButton";
import FaucetButton from "./buttons/FaucetButton";

class AccountExplorer extends TabbedExplorer {
  static defaultProps = {
    route: "account",
    Page: AccountPage,
    valueFormatter: value => value.toLowerCase(),
    ToolbarButtons: ({ value, ...otherProps }) => (
      <>
        <TransferButton from={value} {...otherProps} />
        <FaucetButton address={value} {...otherProps} />
      </>
    ),
  };

  constructor(props) {
    super(props);
    this.keypairs = {};
    props.cacheLifecycles.didRecover(this.checkLocation);
    this.contextMenu = [
      {
        text: "Close",
        onClick: this.closeCurrent,
      },
      {
        text: "Close Others",
        onClick: this.closeOthers,
      },
    ];
  }

  closeCurrent = current => {
    const { onCloseTab } = this.tabs.current.tabs.current;
    onCloseTab(current);
  };

  closeOthers = currentTab => {
    const { onCloseTab, allTabs } = this.tabs.current.tabs.current;
    const shouldCloseTabs = allTabs.filter(tab => tab.key !== currentTab.key);

    shouldCloseTabs.forEach(tab => {
      onCloseTab(tab);
    });
  };

  componentDidMount() {
    this.init();
    keypairManager.onUpdated(() => this.forceUpdate());
  }

  componentDidUpdate(props) {
    if (this.props.network !== props.network) {
      this.init();
    }

    if (this.props.match?.params?.value !== props.match?.params?.value) {
      this.checkLocation();
    }
  }

  init = () => {
    const { history, route, network, accounts, match } = this.props;
    const value = accounts.getIn([network, "selected"]);
    if (match?.params && value !== match?.params?.value) {
      history.push(value ? `/${route}/${value}` : `/${route}`);
    }
    const tabs = accounts.getIn([network, "tabs"])?.toArray() || [];
    this.initialize({ value, tabs, subroute: network });
  };

  checkLocation = () => {
    const value = this.props.match?.params?.value || "";
    return value && this.openTab(value);
  };

  render() {
    const { history, route, network, uiState, accounts, tokens, valueFormatter } = this.props;

    if (network === "dev" && !uiState.get("localNetwork")) {
      return (
        <Screen>
          <h4 className="display-4">No Network</h4>
          <p className="lead">
            No connected network. Please start a local network or switch to a remote network.
          </p>
          <hr />
          <span>
            <Button color="primary" onClick={() => history.push(`/network/${network}`)}>
              Go to Network
            </Button>
          </span>
        </Screen>
      );
    }

    const starred = accounts.getIn([network, "accounts"])?.toArray() || [];
    const props = {
      starred,
      subroute: network,
      signer: uiState.get("signer"),
      tabContextMenu: this.contextMenu,
      getTabText: tab => {
        const { text, value = "" } = tab;
        if (text) {
          return text;
        }
        const address = valueFormatter(value);
        let tabText = "";
        const tokenInfo = tokens?.getIn([network, address])?.toJS();
        const name = keypairManager.getName(address);
        if (name) {
          tabText = (
            <div key={`addr-${address}`} className="d-flex flex-row align-items-center">
              <i className="fas fa-map-marker-alt text-muted mr-1" />
              {name}
            </div>
          );
        } else if (tokenInfo) {
          const icon = tokenInfo.icon ? (
            <img src={tokenInfo.icon} className="token-icon-xs mr-1" />
          ) : (
            <i className="fas fa-coins text-muted mr-1" />
          );
          tabText = (
            <div key={`token-${address}`} className="d-flex flex-row align-items-center">
              {icon}
              {tokenInfo.symbol}
            </div>
          );
        } else if (address.length < 10) {
          tabText = <code>{address}</code>;
        } else {
          tabText = (
            <code>
              {address.substr(0, 6)}...{address.slice(-4)}
            </code>
          );
        }
        return tabText;
      },
      onValueUpdate: value => {
        redux.dispatch("SELECT_ACCOUNT", { network, account: value });
        history.push(`/${route}/${value}`);
      },
      onTabsUpdate: tabs => {
        redux.dispatch("SET_ACCOUNT_TABS", { network, tabs });
      },
      onStarredUpdate: starred => {
        redux.dispatch("SET_STARRED", { network, starred });
      },
    };

    return super.render(props);
  }
}

export default connect(["uiState", "network", "accounts", "tokens"])(withRouter(AccountExplorer));
