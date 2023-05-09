import React, { PureComponent } from "react";

import { List } from "immutable";
import redux, { connect } from "~/base-components/redux";

import headerActions, { Header, NavGuard } from "~/ligo-components/ligo-header";
import { networkManager } from "~/ligo-components/ligo-network";
import { actions } from "~/base-components/workspace";
import keypairManager from "~/base-components/keypair";

import LigoSdk from "~/ligo-components/ligo-sdk";

keypairManager.kp = LigoSdk.kp;

networkManager.addSdk(LigoSdk, LigoSdk.networks);
networkManager.addSdk(LigoSdk, LigoSdk.customNetworks);

function networkCustomGroupData(networkMap) {
  return Object.keys(networkMap)
    .map((name) => ({
      group: "Others",
      icon: "fas fa-vial",
      id: name,
      name,
      fullName: name,
      notification: `Switched to <b>${name}</b>.`,
      url: networkMap[name].url,
      networkId: networkMap[name]?.networkId || name,
      chainId: networkMap[name]?.chainId || "",
    }))
    .sort((a, b) => a.name.localeCompare(b.name));
}

const customeNetworkGroup = networkCustomGroupData(redux.getState()?.customNetworks.toJS());
if (customeNetworkGroup.length > 0) networkManager.addSdk(LigoSdk, customeNetworkGroup);

class HeaderWithRedux extends PureComponent {
  state = {
    interval: null,
    isOpenKeypair: false,
  };

  constructor(props) {
    super(props);
    actions.headerRef = this;
  }

  componentDidMount() {
    actions.history = this.props.history;
    headerActions.history = this.props.history;
    this.refresh();
    this.navGuard = new NavGuard(this.props.history);

    const networkId = this.props.network;
    if (!networkId || (networkManager.network && networkId === networkManager.network.id)) return;
    const matchedNet = networkManager.findChainById(networkId);
    if (!matchedNet || networkId === "custom") return;
    networkManager.setNetwork(matchedNet);
  }

  componentDidUpdate(prevProps) {
    if (prevProps.network !== this.props.network) {
      const networkId = this.props.network;
      if (!networkId || (networkManager.network && networkId === networkManager.network.id)) return;
      const matchedNet = networkManager.findChainById(networkId);
      if (!matchedNet || networkId === "custom") return;
      networkManager.setNetwork(matchedNet);
    }
  }

  openKeypair() {
    this.setState({ isOpenKeypair: true });
  }

  async refresh() {
    const customeRefreshNetworkGroup = networkCustomGroupData(this.props.customNetworks.toJS());
    if (JSON.stringify(customeRefreshNetworkGroup) !== JSON.stringify(customeNetworkGroup))
      networkManager.addSdk(LigoSdk, customeRefreshNetworkGroup);
  }

  setNetwork(options) {
    if (!networkManager.network && networkManager.networks.length) {
      networkManager.setNetwork(networkManager.networks[0], options);
    }
  }

  getTestNetworks = (group) => {
    return networkManager.networks.filter(
      (item) =>
        item.group === group &&
        item.chainId &&
        (group === "Others" ? true : item.fullName.includes("Testnet"))
    );
  };

  groupedNetworks = (networksByGroup) => {
    const networkList = [];
    const groups = networksByGroup.toJS();
    const keys = Object.keys(groups);
    keys.forEach((key, index) => {
      groups[key].forEach((network) => {
        network.testnet = [];
        if (
          network.name === "Mainnet" ||
          network.id === "dev" ||
          network.fullName === "Custom Network"
        ) {
          if (network.id !== "dev") {
            network.testnet = this.getTestNetworks(network.group);
          }
          networkList.push(network);
        }
      });
      if (index === keys.length - 2) {
        networkList.push({ divider: true });
      }
    });
    return networkList;
  };

  render() {
    console.debug("[render] HeaderWithRedux");
    const { uiState, profile, projects, network } = this.props;

    const selectedProject = projects.get("selected")?.toJS() || {};

    const networkList = List(networkManager.networks);
    const networkGroups = networkList.groupBy((n) => n.group);
    const groupedNetworks = this.groupedNetworks(networkGroups);
    const selectedNetwork = networkList.find((n) => n.id === network) || {};

    return (
      <Header
        profile={profile}
        projects={projects}
        selectedProject={selectedProject}
        network={selectedNetwork}
        networkList={groupedNetworks}
        uiState={this.props.uiState}
        customNetworks={this.props.customNetworks}
        customNetworkModalStatus={this.props.customNetworkModalStatus}
        onCancelKp={() => this.setState({ isOpenKeypair: false })}
        isOpenKeypair={this.state.isOpenKeypair}
      />
    );
  }
}

export default connect([
  "uiState",
  "profile",
  "projects",
  "network",
  "customNetworks",
  "customNetworkModalStatus",
])(HeaderWithRedux);
