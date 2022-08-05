import React, { PureComponent } from "react";

import { Modal, DebouncedFormGroup, FormGroup, Label } from "~/base-components/ui-components";

import redux from "~/base-components/redux";
import notification from "~/base-components/notification";

import networkManager from "../networkManager";

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
    if (!status) {
      this.tryCreateSdk({ ...option, notify: false });
    } else {
      if (modify) {
        redux.dispatch("MODIFY_CUSTOM_NETWORK", { name: this.name, option });
      } else {
        redux.dispatch("ADD_CUSTOM_NETWORK", option);
      }
      this.setState({ pending: false, status: null });
      this.modal.current.closeModal();
    }
  };

  render() {
    const { placeholder = "http(s)://..." } = this.props;
    const { modify, pending, status, option } = this.state;

    return (
      <Modal
        ref={this.modal}
        title={`${modify ? "Modify" : "New"} Custom Network Connection`}
        pending={pending && "Trying to connect..."}
        textConfirm={status ? (modify ? "Update Network" : "Add Network") : "Check Network"}
        onConfirm={this.onConfirm}
      >
        <DebouncedFormGroup
          ref={this.input}
          label="Name"
          maxLength="50"
          value={option.name}
          onChange={(name) => this.setState({ option: { ...option, name } })}
        />
        <DebouncedFormGroup
          label="URL of node rpc"
          placeholder={placeholder}
          maxLength="300"
          value={option.url}
          onChange={(url) => this.setState({ status: null, option: { ...option, url } })}
        />
        {status && (
          <FormGroup>
            <Label>Network info</Label>
            <pre className="text-body pre-wrap break-all small user-select mb-0">
              {JSON.stringify(status, null, 2)}
            </pre>
          </FormGroup>
        )}
      </Modal>
    );
  }
}
