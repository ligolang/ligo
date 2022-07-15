import React, { PureComponent } from "react";

import { Modal } from "~/base-components/ui-components";
import fileOps from "~/base-components/file-ops";

import globalModalManager from "./globalModalManager";

export default class AboutModal extends PureComponent {
  constructor(props) {
    super(props);
    this.modal = React.createRef();
    this.state = {
      debugCounter: 0,
      appVersion: "",
    };
  }

  async componentDidMount() {
    globalModalManager.aboutModal = this;
    const appVersion = await fileOps.getAppVersion();
    this.setState({ appVersion });
  }

  openModal() {
    this.modal.current.openModal();
  }

  handleClick() {}

  render() {
    const { appVersion } = this.state;

    return (
      <Modal ref={this.modal} title="About" textCancel="Close">
        <div className="d-flex flex-column align-items-center justify-content-center">
          <img
            src={this.props.icon}
            style={{ background: "transparent", width: "100px" }}
            onClick={() => this.handleClick()}
          />
          <p className="mt-3">
            <span className="h4">
              <b>{process.env.PROJECT_NAME}</b>
            </span>{" "}
            v{appVersion}
          </p>

          <h5 className="small-caps mt-4">contact us</h5>
          <p>
            Website:{" "}
            <a href="#" onClick={() => fileOps.openLink("https://www.obsidians.io")}>
              https://www.obsidians.io
            </a>
          </p>
          {this.props.children}
        </div>
      </Modal>
    );
  }
}
