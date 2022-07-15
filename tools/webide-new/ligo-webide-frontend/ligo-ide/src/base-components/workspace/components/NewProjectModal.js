import React, { PureComponent } from "react";

import { Modal, DebouncedFormGroup, DropdownInput } from "~/base-components/ui-components";

import fileOps from "~/base-components/file-ops";
import notification from "~/base-components/notification";

import ProjectManager from "../ProjectManager";
import actions from "../actions";

export default class NewProjectModal extends PureComponent {
  constructor(props) {
    super(props);

    this.state = {
      name: "",
      template: props.defaultTemplate,
      creating: false,
    };

    this.modal = React.createRef();
    this.path = fileOps.pathHelper;

    actions.newProjectModal = this;
  }

  openModal() {
    const { defaultTemplate } = this.props;
    this.setState({
      template: defaultTemplate,
      creating: false,
    });
    this.forceUpdate();
    this.modal.current.openModal();
    return new Promise(resolve => {
      this.onConfirm = resolve;
    });
  }

  onCreateProject = async () => {
    this.setState({ creating: true });

    const { name, template } = this.state;

    const created = await this.createProject(name, template);

    if (created) {
      this.modal.current.closeModal();
      this.onConfirm(created);
      this.setState({
        name: "",
        template: this.props.defaultTemplate,
        creating: false,
      });
    } else {
      this.setState({ creating: false });
    }
  };

  async createProject(name, template) {
    try {
      const Manager = ProjectManager.Local;
      const created = await Manager.createProject(name, template);
      notification.success("Successful", `New project <b>${name}</b> is created.`);
      return created;
    } catch (e) {
      notification.error("Cannot Create the Project", e.message);
      return undefined;
    }
  }

  renderTemplate() {
    const { templates } = this.props;
    const { template } = this.state;
    return (
      <DropdownInput
        label="Template"
        options={templates}
        placeholder="(Please select a template)"
        value={template}
        onChange={template => this.setState({ template })}
      />
    );
  }

  render() {
    const { name, creating } = this.state;

    return (
      <Modal
        ref={this.modal}
        title="Create a New Project"
        textConfirm="Create Project"
        onConfirm={this.onCreateProject}
        pending={creating && "Creating..."}
        confirmDisabled={!name}
      >
        <DebouncedFormGroup
          label="Project name"
          value={name}
          onChange={name => this.setState({ name })}
        />
        {this.renderTemplate(true)}
      </Modal>
    );
  }
}

const templates = [
  { id: "empty", display: "Empty Project" },
  { id: "increment", display: "Increment" },
  { id: "id", display: "ID" },
  { id: "hashlock", display: "Hashlock Contract" },
];

NewProjectModal.defaultProps = {
  defaultTemplate: "increment",
  templates,
};
