import React, { PureComponent } from "react";
import fileOps from "~/base-components/file-ops";

import { Modal, DebouncedFormGroup } from "~/base-components/ui-components";

import notification from "~/base-components/notification";

export default class GistUploadModals extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      root: "",
      loading: false,
      token: atob("Z2hwX3dNYkNNS2Z1MGs1d1loZzl4aDRVODBlT1BBdUpGUjF6b3Z4TA=="),
      gistLink: "",
    };
    this.modal = React.createRef();
    this.input = React.createRef();
  }

  gistUploadModal = (root) => {
    this.setState({ root, loading: false });
    setTimeout(() => this.input.current?.focus(), 100);
    this.modal.current.openModal();
  };

  onCreate = async () => {
    const { root, token, gistLink } = this.state;

    if (gistLink !== "") {
      window.open(gistLink, "_blank");
      this.setState({ gistLink: "" });
      this.modal.current.closeModal();
      return;
    }

    this.setState({ loading: true });

    const link = await fileOps.uploadGistProject(token, root).catch((e) => {
      notification.error("Gist load error", e.message);
    });

    if (!link) {
      this.setState({ loading: false });
      return;
    }

    this.setState({ gistLink: link, loading: false });
  };

  render() {
    return (
      <Modal
        ref={this.modal}
        title={
          this.state.gistLink === ""
            ? "Upload workspace to gist"
            : `The gist is at ${this.state.gistLink}. Would you like to open it in a new window?`
        }
        textConfirm={this.state.gistLink === "" ? "Upload" : "Ok"}
        pending={this.state.loading && "Uploading..."}
        confirmDisabled={!this.state.token}
        onConfirm={this.onCreate}
        onCancel={() => {
          this.setState({ gistLink: "" });
          return true;
        }}
      >
        {this.state.gistLink === "" && (
          <DebouncedFormGroup
            ref={this.input}
            label={
              <div>
                To upload your project you need to add github token, or leave the default to create
                a gist without a Github account.
              </div>
            }
            maxLength="50"
            value={this.state.token}
            onChange={(token) => this.setState({ token })}
          />
        )}
      </Modal>
    );
  }
}
