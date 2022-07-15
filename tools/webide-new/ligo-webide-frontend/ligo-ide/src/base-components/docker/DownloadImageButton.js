import React, { PureComponent } from "react";

import {
  Modal,
  DropdownToggle,
  DropdownMenu,
  DropdownItem,
  UncontrolledButtonDropdown,
} from "~/base-components/ui-components";

import Terminal from "~/base-components/terminal";
import notification from "~/base-components/notification";

import DockerImageChannel from "./DockerImageChannel";

export default class DownloadImageButton extends PureComponent {
  constructor(props) {
    super(props);

    this.state = {
      loading: false,
      downloading: false,
      versions: [],
      downloadVersion: "",
    };
    this.modal = React.createRef();
  }

  get channel() {
    return this.props.channel || new DockerImageChannel(this.props.imageName);
  }

  componentDidMount() {
    this.fetchRemoteVersions();
  }

  fetchRemoteVersions = async () => {
    this.setState({ loading: true });
    let versions;
    try {
      versions = await this.channel.remoteVersions();
    } catch (e) {
      this.setState({ loading: false });
      notification.error(e.message);
      console.warn(e);
      return;
    }

    this.setState({ loading: false, versions });
  };

  onSelectVersion = downloadVersion => {
    this.setState({ downloadVersion, downloading: true });
    this.modal.current.openModal();
  };

  onDownloaded = ({ code }) => {
    this.setState({ downloading: false });
    if (code) {
      notification.info("Download Task Cancelled");
      return;
    }
    this.modal.current.closeModal();
    this.props.onDownloaded();
  };

  renderVersions = () => {
    const { loading, versions } = this.state;
    if (loading) {
      return (
        <DropdownItem key="icon-loading-versions">
          <i className="fas fa-spin fa-spinner mr-1" />
          Loading...
        </DropdownItem>
      );
    }

    if (!versions.length) {
      return <DropdownItem disabled>(None)</DropdownItem>;
    }

    return versions.map(({ name }) => (
      <DropdownItem key={name} onClick={() => this.onSelectVersion(name)}>
        {name}
      </DropdownItem>
    ));
  };

  render() {
    const { imageName } = this.channel;
    const {
      installDropdownHeader = "Available Versions",
      downloadingTitle = `Downloading ${imageName}`,
      size,
      color = "secondary",
      right,
      extraFlags,
    } = this.props;

    const logId = `terminal-docker-${imageName}`;
    let cmd = `docker pull ${imageName}:${this.state.downloadVersion}`;
    if (extraFlags) {
      cmd = `${cmd} ${extraFlags}`;
    }

    let title;

    if (this.state.downloading) {
      title = (
        <div key="icon-downloading">
          <i className="fas fa-spinner fa-spin mr-2" />
          {downloadingTitle} {this.state.downloadVersion}...
        </div>
      );
    } else {
      title = <div>{downloadingTitle.replace("ing", "")} Cancelled</div>;
    }
    return (
      <>
        <UncontrolledButtonDropdown size={size}>
          <DropdownToggle caret color={color} onClick={() => this.fetchRemoteVersions()}>
            <i className="fas fa-download mr-1" />
            Install
          </DropdownToggle>
          <DropdownMenu right={right}>
            <DropdownItem header>{installDropdownHeader}</DropdownItem>
            {this.renderVersions()}
          </DropdownMenu>
        </UncontrolledButtonDropdown>
        <Modal ref={this.modal} title={title}>
          <div className="rounded overflow-hidden">
            <Terminal
              active
              logId={logId}
              height="300px"
              cmd={cmd}
              onCmdExecuted={this.onDownloaded}
            />
          </div>
        </Modal>
      </>
    );
  }
}
