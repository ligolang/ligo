import React, { PureComponent } from "react";

import redux from "~/base-components/redux";
import Navbar from "~/base-components/navbar";
import {
  navbarItem,
  OpenProjectModal,
  NewProjectModal,
  RenameProjectModal,
} from "~/base-components/workspace";
import { networkManager } from "~/ligo-components/eth-network";

import { default as CustomNetworkModal } from "~/ligo-components/eth-network/CustomNetwork/CustomNetworkModal";

export default class Header extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      keypairs: [],
      showCustomNetworkModal: false,
    };
    this.openProjectModalRef = React.createRef();
    this.newProjectModalRef = React.createRef();
    this.customModal = React.createRef();
  }

  componentDidUpdate(prevProps) {
    if (
      this.props.customNetworkModalStatus &&
      this.props.customNetworkModalStatus !== this.state.showCustomNetworkModal
    ) {
      this.setState({ showCustomNetworkModal: true });
      this.customModal.current?.openModal();
    }

    if (
      !this.props.customNetworkModalStatus &&
      this.props.customNetworkModalStatus !== this.state.showCustomNetworkModal
    ) {
      this.setState({ showCustomNetworkModal: false });
    }
  }

  updateKeypairs = (keypairs) => this.setState({ keypairs });

  render() {
    const {
      profile,
      projects,
      selectedProject,
      network,
      networkList,
      customNetworks,
      uiState,
      customNetworkModalStatus,
      onCancelKp,
      isOpenKeypair,
    } = this.props;

    const username = projects.get("selected")?.toJS()?.author;
    const navbarLeft = [navbarItem(projects, selectedProject, username)];

    const networkReplaceName = { ...network, name: network.name || network.fullName };
    const networkNavbarItem = {
      route: "network",
      title: "Network",
      icon: network.icon,
      selected: networkReplaceName,
      dropdown: networkList,
      onClickItem: (_, nw) => {
        if (nw.id === "custom") redux.dispatch("CUSTOM_MODAL_STATUS", true);
        if (nw.id !== "custom") networkManager.setNetwork(nw);
      },
    };

    const navbarRight = [networkNavbarItem];

    return (
      <>
        <Navbar
          profile={profile}
          navbarLeft={navbarLeft}
          navbarRight={navbarRight}
          onCancelKp={onCancelKp}
          isOpenKeypair={isOpenKeypair}
        />
        <NewProjectModal ref={this.newProjectModalRef} />
        <RenameProjectModal />
        <OpenProjectModal ref={this.openProjectModalRef} />
        <CustomNetworkModal
          ref={this.customModal}
          networkId={network}
          customNetworks={customNetworks}
          option={uiState.get("customNetworkOption")}
          openModal={this.state.showCustomNetworkModal}
        />
      </>
    );
  }
}
