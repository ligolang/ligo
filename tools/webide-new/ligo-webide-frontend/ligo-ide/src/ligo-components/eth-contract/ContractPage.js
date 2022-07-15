import React, { PureComponent } from "react";

import {
  Screen,
  SplitPane,
  LoadingScreen,
  Button,
  UncontrolledButtonDropdown,
  DropdownToggle,
  DropdownMenu,
  DropdownItem,
} from "~/base-components/ui-components";

import { networkManager } from "~/ligo-components/eth-network";
import redux from "~/base-components/redux";
import { BaseProjectManager } from "~/base-components/workspace";

import ContractActions from "./ContractActions";
import ContractViews from "./ContractViews";
import ContractEvents from "./ContractEvents";
import AbiStorageModal from "./AbiStorage/AbiStorageModal";

export default class ContractPage extends PureComponent {
  constructor(props) {
    super(props);
    this.abiStorageModal = React.createRef();

    this.state = {
      error: null,
      errorType: null,
      tokenInfo: null,
      abi: {},
      abis: [],
      projectAbis: null,
      selectedAbi: null,
      account: null,
      loading: true,
    };
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
    if (prevProps.projectRoot !== this.props.projectRoot) {
      this.loadProjectAbis();
    }
  }

  componentDidRecover = () => {
    this.props.onDisplay(this);
  };

  refresh = async () => {
    this.setState({
      loading: true,
      error: null,
      abi: {},
      abis: [],
      selectedAbi: null,
      account: null,
      errorType: null,
    });

    await new Promise(resolve => setTimeout(resolve, 10));

    const { value } = this.props;

    if (!value) {
      this.setState({ loading: false, error: "No address entered." });
      return;
    }

    try {
      const abiData = this.getAbiData(value);
      if (abiData) {
        this.setState({ loading: false, abi: abiData });
        return;
      }
    } catch (e) {
      this.setState({ loading: false, error: e.message });
    }

    let account;
    try {
      account = await networkManager.sdk.accountFrom(value);
      this.setState({ account });
    } catch (e) {
      this.setState({ loading: false, error: e.message });
      return;
    }

    if (account.codeHash) {
      this.getTokenInfo(account);
    } else {
      this.setState({
        loading: false,
        error: "No contract deployed.",
        tokenInfo: null,
      });
      return;
    }

    try {
      const abiData = this.getAbiData(account.codeHash);
      if (abiData) {
        this.setState({ loading: false, abi: abiData });
        return;
      }
    } catch (e) {
      this.setState({ loading: false, error: e.message });
    }

    this.loadProjectAbis();

    this.setState({
      loading: false,
      error: (
        <span>
          No ABI for code hash <code>{account.codeHash}</code>.
        </span>
      ),
      errorType: "ABI_NOT_FOUND",
      abis: Object.entries(redux.getState().abis.toJS()).map(item => ({
        id: item[0],
        name: item[1].name,
        abi: JSON.parse(item[1].abi),
      })),
    });
  };

  getTokenInfo = async account => {
    const tokenInfo = await networkManager.sdk.getTokenInfo(account.address);
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
  };

  loadProjectAbis = async () => {
    const projectAbis = await BaseProjectManager.instance?.readProjectAbis();
    this.setState({
      projectAbis: projectAbis?.map(item => ({
        ...item,
        id: item.pathInProject || item.contractPath,
      })),
    });
  };

  getAbiData(codeHash) {
    const abiData = redux
      .getState()
      .abis.get(codeHash)
      ?.toJS();
    if (!abiData) {
      return;
    }
    try {
      abiData.abi = JSON.parse(abiData.abi === "{}" ? "[]" : abiData.abi);
    } catch {
      throw new Error("Invalid ABI structure.");
    }
    return abiData;
  }

  async openAbiStorageModal(codeHash) {
    await this.abiStorageModal.current.newAbi("", codeHash);
    this.refresh();
  }

  renderAbiDropdownItem = abis => {
    if (!abis) {
      return null;
    }
    if (!abis.length) {
      return <DropdownItem disabled>(No ABI found)</DropdownItem>;
    }
    return abis.map(item => {
      return (
        <DropdownItem key={item.id} onClick={() => this.setState({ abi: item, error: null })}>
          <b>{item.name}</b>
          <div className="text-muted small">
            <code>{item.id}</code>
          </div>
        </DropdownItem>
      );
    });
  };

  renderAbisFromProject = () => {
    const abis = this.state.projectAbis;
    if (!abis) {
      return null;
    }

    return (
      <UncontrolledButtonDropdown className="mr-2 mb-2">
        <DropdownToggle color="primary" caret>
          Select from the current project
        </DropdownToggle>
        <DropdownMenu className="dropdown-menu-sm" style={{ maxHeight: 240 }}>
          {this.renderAbiDropdownItem(abis)}
        </DropdownMenu>
      </UncontrolledButtonDropdown>
    );
  };

  separateAbi = abi => {
    const functions = abi.abi.filter(item => item.type === "function");
    const events = abi.abi.filter(item => item.type === "event");
    const actions = functions.filter(item => ["view", "pure"].indexOf(item.stateMutability) === -1);
    const views = functions.filter(item => ["view", "pure"].indexOf(item.stateMutability) > -1);
    return { actions, views, events };
  };

  render() {
    const { subroute: network, value, signer } = this.props;
    const { error, abi, account, errorType } = this.state;

    if (!networkManager.sdk) {
      return null;
    }

    if (!value) {
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
      if (account && errorType && errorType === "ABI_NOT_FOUND") {
        return (
          <Screen>
            <h4 className="display-4">ABI Not Found</h4>
            <p>
              There is no associated ABI for the current contract at <kbd>{account.address}</kbd>{" "}
              with code hash <kbd>{account.codeHash}</kbd>
            </p>
            <hr />
            <div className="d-flex flex-wrap align-items-start">
              <UncontrolledButtonDropdown className="mr-2 mb-2">
                <DropdownToggle color="primary" caret>
                  Select from <b>ABI Storage</b>
                </DropdownToggle>
                <DropdownMenu className="dropdown-menu-sm" style={{ maxHeight: 240 }}>
                  {this.renderAbiDropdownItem(this.state.abis)}
                </DropdownMenu>
              </UncontrolledButtonDropdown>
              <UncontrolledButtonDropdown className="mr-2 mb-2">
                <DropdownToggle color="primary" caret>
                  Add ABI to <b>ABI Storage</b>
                </DropdownToggle>
                <DropdownMenu>
                  <DropdownItem onClick={() => this.openAbiStorageModal(account.codeHash)}>
                    Add for code hash{" "}
                    <small>
                      <code>
                        {account.codeHash.substr(0, 6)}...
                        {account.codeHash.substr(-4)}
                      </code>
                    </small>
                  </DropdownItem>
                  <DropdownItem onClick={() => this.openAbiStorageModal(account.address)}>
                    Add for contract address{" "}
                    <small>
                      <code>
                        {account.address.substr(0, 6)}...
                        {account.address.substr(-4)}
                      </code>
                    </small>
                  </DropdownItem>
                </DropdownMenu>
              </UncontrolledButtonDropdown>
              {this.renderAbisFromProject()}
            </div>
            <AbiStorageModal ref={this.abiStorageModal} />
          </Screen>
        );
      }
      return (
        <Screen>
          <h4 className="display-4">Error</h4>
          <p className="lead">{error}</p>
        </Screen>
      );
    }

    const contractInstance = networkManager.sdk.contractFrom({
      ...abi,
      address: value,
    });
    const { actions, views, events } = this.separateAbi(abi);

    return (
      <div className="d-flex p-relative h-100 w-100">
        <SplitPane split="vertical" defaultSize={320} minSize={200}>
          <ContractActions
            network={network}
            value={value}
            actions={actions}
            contract={contractInstance}
            signer={signer}
            // history={contractCalls.getIn(['action', 'history'])}
            // bookmarks={contractCalls.getIn(['action', 'bookmarks'])}
          />
          <SplitPane split="vertical" defaultSize={320} minSize={200}>
            <ContractViews
              network={network}
              value={value}
              actions={views}
              contract={contractInstance}
              // history={contractCalls.getIn(['table', 'history'])}
              // bookmarks={contractCalls.getIn(['table', 'bookmarks'])}
            />
            <ContractEvents value={value} abi={events} contract={contractInstance} />
          </SplitPane>
        </SplitPane>
      </div>
    );
  }
}
