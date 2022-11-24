import React, { PureComponent } from "react";
import headerActions from "~/ligo-components/eth-header";

import redux from "~/base-components/redux";
import notification from "~/base-components/notification";

import {
  Modal,
  Table,
  Button,
  IconButton,
  DeleteButton,
  UncontrolledTooltip,
} from "~/base-components/ui-components";
import networkManager from "../networkManager";
import NewCustomNetworkModal from "./NewCustomNetworkModal";

export default class CustomNetworkModal extends PureComponent {
  constructor(props) {
    super(props);
    this.state = { connecting: "", customNetworkItem: null };
    this.modal = React.createRef();
    this.newConnectionModal = React.createRef();
    this.deleteModal = React.createRef();
  }

  openModal = (customNetwork = {}) => {
    this.setState({ connecting: "" });
    this.modal.current?.openModal();
  };

  openNewConnectionModal = (modify, option) => {
    this.newConnectionModal.current?.openModal(modify, option);
  };

  delete = (item) => {
    this.deleteModal.current?.openModal();
    this.setState({
      customNetworkItem: item,
    });
  };

  deleteConfirm = async () => {
    const currentNetwork = this.state.customNetworkItem?.name;
    this.deleteModal.current.closeModal();
    const customNetworkMap = redux.getState().customNetworks.toJS();

    if (redux.getState().networkConnect && customNetworkMap[currentNetwork].active) {
      this.modal.current.closeModal();
      networkManager.setNetwork(networkManager.networks[0], {
        redirect: false,
        notify: false,
      });
    }
    networkManager.deleteNetwork(currentNetwork);
    redux.dispatch("REMOVE_CUSTOM_NETWORK", currentNetwork);
  };

  connect = async (option) => {
    try {
      this.setState({ connecting: option.name });
      const status = await networkManager.updateCustomNetwork({ ...option, notify: false });
      if (status) {
        redux.dispatch("CHANGE_NETWORK_STATUS", true);
        this.modal.current?.closeModal();
        this.setState({ connecting: "" });
        headerActions.updateNetwork(option.name);
        const connectCustomeNetwork = networkManager.networks?.find(
          (item) => item.id === option.name
        );
        networkManager.setNetwork(connectCustomeNetwork);
        return;
      }
    } catch {}
    notification.error(
      "Network Error",
      "Failed to connect the network. Make sure you entered a valid url for the node RPC."
    );
    this.setState({ connecting: "" });
  };

  renderTableBody = () => {
    const connecting = this.state.connecting;
    const customNetworks = this.props.customNetworks.toArray();
    customNetworks.sort((a, b) => a[0].localeCompare(b[0]));

    if (customNetworks.length) {
      return customNetworks.map(([name, item], i) => {
        const toolTipId = name.replace(/\s/g, "");
        return (
          <tr key={`custom-network-${i}`} className="hover-flex">
            <td className="d-flex">
              <div className="text-overflow-dots" id={`custom-${toolTipId}`}>
                <span>{name}</span>
              </div>
              {name && (
                <UncontrolledTooltip placement="right" target={`custom-${toolTipId}`}>
                  {name}
                </UncontrolledTooltip>
              )}
            </td>
            <td className="text-overflow-dots">{item.get("url")}</td>
            <td align="right">
              <div className="d-flex align-items-center justify-content-between">
                <Button
                  key={connecting === name ? `${name}-connecting` : `${name}-connect`}
                  size="sm"
                  color="success"
                  onClick={() => this.connect(item.toJS())}
                >
                  {connecting === name ? (
                    <>
                      <i className="fas fa-spin fa-spinner mr-1" />
                      Connecting...
                    </>
                  ) : (
                    "Connect"
                  )}
                </Button>
                {connecting !== name && (
                  <div className="d-flex hover-show">
                    <IconButton
                      color="transparent"
                      className="text-muted"
                      onClick={() => this.openNewConnectionModal(true, item.toJS())}
                      icon="fas fa-pencil-alt"
                    />
                    <IconButton
                      color="transparent"
                      className="ml-1 text-muted delete-test"
                      onClick={() => this.delete(item.toJS())}
                    />
                  </div>
                )}
              </div>
            </td>
          </tr>
        );
      });
    }
    return (
      <tr key="custom-network-none">
        <td align="middle" colSpan={3}>
          (None)
        </td>
      </tr>
    );
  };

  onClosed = () => redux.dispatch("CUSTOM_MODAL_STATUS", false);

  render() {
    const networkConnectingText = "it will be disconnected immediately and cannot be restored.";
    const networkNotConnectedText = "it cannot be restored.";
    const currentNetwork = this.state.customNetworkItem?.name;
    const customNetworkMap = redux.getState().customNetworks.toJS();
    const connected = redux.getState().networkConnect && customNetworkMap[currentNetwork]?.active;

    return (
      <>
        <Modal
          ref={this.modal}
          title="Custom Network"
          textActions={["New Connection"]}
          onClosed={this.onClosed}
          onActions={[() => this.openNewConnectionModal()]}
        >
          <Table
            tableSm
            TableHead={
              <tr>
                <th style={{ width: "15%" }}>name</th>
                <th style={{ width: "45%" }}>rpc url</th>
                <th />
              </tr>
            }
          >
            {this.renderTableBody()}
          </Table>
        </Modal>
        <Modal
          ref={this.deleteModal}
          title="Delete Custom Network"
          size="md"
          textConfirm="Delete"
          noCancel
          onConfirm={this.deleteConfirm}
        >
          <div>
            Are you sure you want to delete{" "}
            <kbd className="color-danger">{this.state.customNetworkItem?.name} </kbd> ? Once
            deleted,
            {connected ? networkConnectingText : networkNotConnectedText}
          </div>
        </Modal>
        <NewCustomNetworkModal ref={this.newConnectionModal} />
      </>
    );
  }
}
