import React, { PureComponent } from "react";

import debounce from "lodash/debounce";
import { Modal, DebouncedFormGroup, Button } from "~/base-components/ui-components";

import platform from "~/base-components/platform";

import actions from "../actions";

class DeleteButton extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      type: "Delete Project",
      deleting: false,
      inputPlaceholder: "",
      name: "",
      projectName: "",
      projectRoot: "",
      confirmDisableStatus: true,
    };
    this.modal = React.createRef();
    this.input = React.createRef();
  }

  openDeleteProjectModal = () => {
    const { projectManager } = this.props.context;
    const inputPlaceholder = `Please type ${
      platform.isWeb ? projectManager.projectRoot : projectManager.projectName
    } to confirm`;
    this.setState({
      projectRoot: projectManager.projectRoot,
      projectName: projectManager.projectName,
      deleting: false,
      inputPlaceholder,
      projectManager,
    });
    setTimeout(() => this.input.current?.focus(), 100);
    this.modal.current.openModal();
  };

  changeVal = name => {
    const { projectName, projectRoot } = this.state;
    const currentName = platform.isWeb ? projectRoot : projectName;
    name = name.trim();
    const confirmDisableStatus = name != currentName;
    this.setState({ name, confirmDisableStatus });
  };

  deleteProject = async () => {
    this.setState({ deleting: true });
    const { projectManager } = this.props.context;
    const name = projectManager.projectName;
    await projectManager.deleteProject();
    await actions.removeProject({ id: name, name, type: "delete" });
    this.setState({ deleting: false });
    this.modal.current?.closeModal();
  };

  render() {
    const { projectManager, projectRoot } = this.props.context;

    if (!projectManager.remote) {
      return null;
    }

    return (
      <>
        <h4 className="mt-4">Others</h4>
        <Button color="danger" onClick={this.openDeleteProjectModal}>
          Delete Project
        </Button>
        <Modal
          ref={this.modal}
          title={this.state.type}
          textConfirm="Delete"
          colorConfirm="danger"
          pending={this.state.deleting && "Deleting..."}
          noCancel
          headerCancelIcon={this.state.deleting}
          footerCancelIcon={this.state.deleting}
          confirmDisabled={this.state.confirmDisableStatus}
          onConfirm={this.deleteProject}
        >
          <DebouncedFormGroup
            ref={this.input}
            label={
              <div>
                You are about to permanently delete this project. This operation <b>CANNOT</b> be
                undone!
                <div>
                  Type <kbd>{platform.isWeb ? this.state.projectRoot : this.state.projectName}</kbd>{" "}
                  to confirm
                </div>
              </div>
            }
            placeholder={this.state.inputPlaceholder}
            maxLength="100"
            value={this.state.name}
            onChange={this.changeVal}
          />
          <div
            className="color-danger"
            hidden={!this.state.name || (this.state.name && !this.state.confirmDisableStatus)}
          >
            Project name does not match
          </div>
        </Modal>
      </>
    );
  }
}

export default class AbstractProjectSettingsTab extends PureComponent {
  static DeleteButton = DeleteButton;

  onChangeHandlers = {};

  debouncedUpdate = debounce(() => this.forceUpdate(), 500, {
    leading: true,
    trailing: false,
  }).bind(this);

  onChange = key => {
    if (!this.onChangeHandlers[key]) {
      this.onChangeHandlers[key] = value => {
        this.context.projectSettings?.set(key, value);
      };
    }
    return this.onChangeHandlers[key];
  };
}
