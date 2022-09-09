import React, { PureComponent } from "react";

import { Modal } from "~/base-components/ui-components";

import notification from "~/base-components/notification";

import RpcActionForm from "./RpcActionForm";
import networkManager from "../networkManager";

export default class RpcClientModal extends PureComponent {
  constructor(props) {
    super(props);
    this.modal = React.createRef();
  }

  openModal = () => {
    if (!networkManager.sdk) {
      notification.error(
        "No Network",
        "No connected network. Please start a local network or switch to a remote network."
      );
      return;
    }
    this.modal.current.openModal();
  };

  render() {
    return (
      <Modal ref={this.modal} scrollable title="RPC Client" textCancel="Close">
        <RpcActionForm />
      </Modal>
    );
  }
}
