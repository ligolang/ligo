import React, { PureComponent } from "react";

import { Modal, DebouncedFormGroup } from "~/base-components/ui-components";

import notification from "~/base-components/notification";

export default class RenameModal extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      type: "file",
      loading: false,
      oldName: "",
      name: "",
      baseName: "",
      basePath: "",
    };
    this.modal = React.createRef();
    this.input = React.createRef();
  }

  openModal = ({ type, name, oldPath }) => {
    this.setState({ type, name, oldName: name, loading: false, oldPath });
    setTimeout(() => this.input.current?.focus(), 100);
    this.modal.current.openModal();
  };

  onRename = async () => {
    const { oldPath, name, type } = this.state;
    this.setState({ loading: true });
    try {
      await this.props.projectManager.rename(oldPath, name, { type });
    } catch (e) {
      notification.error("Cannot Rename File", e.message);
      return;
    }
    this.setState({ loading: false });
    this.modal.current.closeModal();
  };

  render() {
    return (
      <Modal
        ref={this.modal}
        title={this.state.type === "file" ? "Rename File" : "Rename Folder"}
        textConfirm="Rename"
        pending={this.state.loading && "Renaming..."}
        confirmDisabled={!this.state.name}
        onConfirm={this.onRename}
      >
        <DebouncedFormGroup
          ref={this.input}
          label={
            <div>
              Rename <kbd>{this.state.oldName}</kbd> to
            </div>
          }
          placeholder="New name"
          maxLength="50"
          value={this.state.name}
          onChange={(name) => this.setState({ name })}
        />
      </Modal>
    );
  }
}
