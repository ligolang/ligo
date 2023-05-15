import React, { PureComponent } from "react";

import { Modal, DebouncedFormGroup, FormGroup, Label } from "~/base-components/ui-components";
import headerActions from "~/ligo-components/ligo-header";
import redux from "~/base-components/redux";
import notification from "~/base-components/notification";

import networkManager from "../networkManager";
import { validUrl, validName } from "~/components/validators";

export default class CustomNetworkModal extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      pending: false,
      status: null,
      modify: false,
      option: {},
    };
    this.modal = React.createRef();
    this.input = React.createRef();
  }

  openModal = (modify = false, option = {}) => {
    this.name = option.name;
    this.setState({ pending: false, status: null, modify, option });
    this.modal.current?.openModal();
    setTimeout(() => this.input.current?.focus(), 100);
  };

  tryCreateSdk = async (option) => {
    this.setState({ pending: true });
    try {
      const status = await networkManager.updateCustomNetwork(option);
      if (status) {
        this.setState({ pending: false, status });
        return;
      }
    } catch {}
    notification.error(
      "Network Error",
      "Failed to connect the network. Make sure you entered a valid url for the node RPC."
    );
    this.setState({ pending: false });
  };

  onConfirm = async () => {
    const { modify, status, option } = this.state;
    const customNetworkMap = redux.getState().customNetworks.toJS();
    const customNetworkNames = Object.keys(customNetworkMap);
    const connected = customNetworkMap[this.name]?.active;

    if (customNetworkNames.includes(option.name) && !modify) {
      notification.error("Invalid network name", `<b>${option.name}</b> alreay exists.`);
      return;
    }
    if (!status) {
      this.tryCreateSdk({ ...option, notify: false });
    } else {
      let hasDuplicated;
      const existChain = networkManager.networks.find((item) => item.chainId === status.chainId);
      if (modify) {
        redux.dispatch("MODIFY_CUSTOM_NETWORK", {
          name: this.name,
          option,
          networkId: existChain?.id,
        });
        if (connected) this.connect(option);
      } else {
        hasDuplicated = networkManager.hasDuplicatedNetwork(option.url);
        hasDuplicated
          ? notification.error(
              "Failed to add network",
              `The RPC url <b>${option.url}</b> alreay exists.`
            )
          : redux.dispatch("ADD_CUSTOM_NETWORK", {
              ...option,
              networkId: existChain?.id,
              chainId: this.state.status?.chainId,
            });
      }
      if (hasDuplicated) return;
      const newNetList = networkManager.getNewNetList();
      networkManager.addNetworks(newNetList);
      this.setState({ pending: false, status: null });
      this.modal.current.closeModal();
    }
  };

  connect = async (option) => {
    try {
      const status = await networkManager.updateCustomNetwork(option);
      if (status) {
        redux.dispatch("UPDATE_UI_STATE", { customNetworkOption: option });
        redux.dispatch("CHANGE_NETWORK_STATUS", true);
        headerActions.updateNetwork(option.name);
        networkManager.setNetwork({
          ...option,
          id: option.name,
        });
        return;
      }
    } catch {}
    notification.error(
      "Network Error",
      "Failed to connect the network. Make sure you entered a valid RPC url"
    );
    redux.dispatch("CHANGE_NETWORK_STATUS", false);
  };

  filterStatus = (status) => {
    if (status && typeof status === "object") {
      for (let i in status) {
        status[i] === "unknown" && delete status[i];
      }
    }
    return status;
  };

  renderNetworkInfo() {
    const networkInfo = this.state.modify
      ? this.filterStatus(this.state.status)
      : {
          chainId: this.state.status?.chainId,
          name: this.state.option?.name,
        };

    const showInfo = !networkInfo ? false : Object.keys(networkInfo).length !== 0;

    return showInfo ? (
      <FormGroup>
        <Label>Network info</Label>
        <pre className="pre-box pre-wrap break-all bg-primary text-body">
          {JSON.stringify(networkInfo, null, 2)}
        </pre>
      </FormGroup>
    ) : null;
  }

  render() {
    const { placeholder = "http(s)://..." } = this.props;
    const { modify, pending, status, option } = this.state;

    return (
      <Modal
        ref={this.modal}
        title={`${modify ? "Modify" : "New"} Custom Network Connection`}
        pending={pending && "try"}
        textConfirm={status ? (modify ? "Update Network" : "Add Network") : "Check Network"}
        onConfirm={this.onConfirm}
        confirmDisabled={!option.name || !!validName(option.name) || !!validUrl(option.url)}
      >
        <DebouncedFormGroup
          ref={this.input}
          label="Name"
          maxLength="50"
          value={option.name}
          onChange={(name) => this.setState({ option: { ...option, name } })}
          validator={validName}
        />
        <DebouncedFormGroup
          label="URL of node rpc"
          placeholder={placeholder}
          maxLength="300"
          value={option.url}
          onChange={(url) => this.setState({ status: null, option: { ...option, url } })}
          validator={validUrl}
        />
        {this.renderNetworkInfo()}
      </Modal>
    );
  }
}
