import React, { PureComponent } from "react";
import { Button, Badge, ListGroupItem } from "~/base-components/ui-components";

import fileOps from "~/base-components/file-ops";

import dockerChannel from "./DockerChannel";
import DownloadImageButton from "./DownloadImageButton";

export default class ListItemDockerImage extends PureComponent {
  mounted = false;

  state = {
    status: "", // '', 'NO_DOCKER', 'NONE', 'INSTALLING', 'INSTALLED'
    versions: [],
  };

  componentDidMount() {
    this.mounted = true;
  }

  componentWillUnmount() {
    this.mounted = false;
  }

  refresh = async () => {
    if (!(await dockerChannel.check())) {
      this.mounted && this.setState({ status: "NO_DOCKER" });
      return;
    }

    let versions;
    try {
      versions = await this.props.channel.versions();
    } catch (e) {
      this.mounted && this.setState({ status: "NO_DOCKER" });
      return;
    }

    if (versions && versions.length) {
      this.mounted &&
        this.setState({
          status: "INSTALLED",
          versions: versions.map((v) => v.Tag),
        });
    } else {
      this.mounted && this.setState({ status: "NONE", versions: [] });
    }
  };

  renderIcon = () => {
    switch (this.state.status) {
      case "":
      case "NO_DOCKER":
      case "NONE":
        return (
          <span key="fail">
            <i className="fas fa-minus-circle mr-2 text-muted" />
          </span>
        );
      case "INSTALLING":
        return (
          <span key="spin">
            <i className="fas fa-spin fa-spinner mr-2 text-muted" />
          </span>
        );
      default:
        return (
          <span key="success">
            <i className="fas fa-check-circle mr-2 text-success" />
          </span>
        );
    }
  };

  renderSubtitle = (subtitle) => {
    switch (this.state.status) {
      case "":
        return <span>Loading...</span>;
      case "NO_DOCKER":
      case "NONE":
      case "INSTALLING":
        return <span>{subtitle}</span>;
      default:
        return (
          <span>
            Installed:{" "}
            {this.state.versions.map((v) => (
              <Badge key={v} className="mr-1">
                {v}
              </Badge>
            ))}
          </span>
        );
    }
  };

  renderButton = () => {
    switch (this.state.status) {
      case "":
        return null;
      case "NO_DOCKER":
        return <Button color="secondary">Need Docker</Button>;
      case "NONE":
        return (
          <DownloadImageButton
            right
            color="primary"
            channel={this.props.channel}
            downloadingTitle={this.props.downloadingTitle}
            onDownloaded={this.props.onInstalled}
          />
        );
      case "INSTALLING":
        return (
          <Button color="primary" disabled>
            <span>
              <i className="fas fa-spin fa-spinner mr-1" />
            </span>
            Installing
          </Button>
        );
      default:
        return <Button color="secondary">Installed</Button>;
    }
  };

  render() {
    const { title, link, subtitle } = this.props;

    return (
      <ListGroupItem>
        <div className="align-items-center d-flex flex-row justify-content-between">
          <div>
            <h5>
              {this.renderIcon()}
              <a href="#" className="text-white" onClick={() => fileOps.openLink(link)}>
                {title}
              </a>
            </h5>
            {this.renderSubtitle(subtitle)}
          </div>
          {this.renderButton()}
        </div>
      </ListGroupItem>
    );
  }
}
