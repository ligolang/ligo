import React, { PureComponent } from "react";

import { Screen, LoadingScreen } from "~/base-components/ui-components";

import redux from "~/base-components/redux";
import { networkManager } from "~/ligo-components/eth-network";

import AccountBalance from "./AccountBalance";
import AccountInfo from "./AccountInfo";
import AccountTransactions from "./AccountTransactions";

export default class AccountPage extends PureComponent {
  state = {
    error: null,
    account: null,
    tokens: [],
    tokenInfo: null,
    loading: true,
  };

  constructor(props) {
    super(props);
    this.accountTransactions = React.createRef();
    props.cacheLifecycles.didRecover(this.componentDidRecover);
  }

  componentDidMount() {
    this.props.onDisplay(this);
    this.refresh();
  }

  componentDidUpdate(prevProps) {
    if (prevProps.value !== this.props.value) {
      this.refresh();
    }
  }

  componentDidRecover = () => {
    this.props.onDisplay(this);
  };

  refresh = async () => {
    this.setState({ loading: true });

    await new Promise((resolve) => setTimeout(resolve, 10));

    const { value } = this.props;

    if (!value) {
      this.setState({ loading: false, error: null, account: null });
      return;
    }

    if (!(await networkManager.sdk?.isValidAddress(value))) {
      this.setState({ loading: false, error: true, account: null });
      return;
    }

    let account;
    try {
      account = await networkManager.sdk.accountFrom(value);
      this.getTokenInfo(account);
      this.setState({ loading: false, error: null, account });
    } catch (e) {
      console.error(e);
      this.setState({ loading: false, error: e.message, account: null });
    }
  };

  getTokenInfo = (account) => {
    networkManager.sdk.getTokens(account.address).then((tokens) => {
      this.setState({ tokens });
    });

    if (!account.codeHash) {
      this.setState({ tokenInfo: null });
      return;
    }

    networkManager.sdk.getTokenInfo(account.address).then((tokenInfo) => {
      this.setState({ tokenInfo });
      if (tokenInfo?.type === "ERC20") {
        redux.dispatch("ADD_TOKEN_INFO", {
          network: networkManager.networkId,
          address: account.address,
          tokenInfo,
        });
      } else {
        redux.dispatch("REMOVE_TOKEN_INFO", {
          network: networkManager.networkId,
          address: account.address,
        });
      }
    });
  };

  render() {
    const { AccountInfo, history } = this.props;
    const { error, account, tokens, tokenInfo } = this.state;

    if (!networkManager.sdk) {
      return null;
    }

    if (!this.props.value) {
      return (
        <Screen>
          <h4 className="display-4">New Page</h4>
          <p className="lead">Please enter an {process.env.CHAIN_NAME} address.</p>
        </Screen>
      );
    }

    if (this.state.loading) {
      return <LoadingScreen />;
    }

    if (error) {
      if (typeof error === "string") {
        return (
          <Screen>
            <h4 className="display-4">Error</h4>
            <p className="lead">{error}</p>
          </Screen>
        );
      }
      return (
        <Screen>
          <h4 className="display-4">Invalid Address</h4>
          <p className="lead">
            <kbd>{this.props.value}</kbd>
          </p>
        </Screen>
      );
    }

    if (!account) {
      return null;
    }

    return (
      <div className="d-flex flex-1 flex-column overflow-auto" key={account.address}>
        <div className="d-flex">
          <div className="col-4 p-0 border-right-black">
            <AccountBalance account={account} tokens={tokens} history={history} />
          </div>
          <div className="col-8 p-0 overflow-auto" style={{ maxHeight: 250 }}>
            <AccountInfo account={account} tokenInfo={tokenInfo} />
          </div>
        </div>
        <div className="d-flex flex-fill overflow-hidden">
          <div className="col-12 p-0 border-top-black overflow-auto">
            <AccountTransactions account={account} ref={this.accountTransactions} />
          </div>
        </div>
      </div>
    );
  }
}

AccountPage.defaultProps = {
  AccountInfo,
};
