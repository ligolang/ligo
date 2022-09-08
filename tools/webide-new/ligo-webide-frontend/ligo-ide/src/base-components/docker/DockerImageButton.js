import React, { PureComponent } from "react";

import { Button, Badge } from "~/base-components/ui-components";

import DockerImageManager from "./DockerImageManager";

export default class DockerImageButton extends PureComponent {
  constructor(props) {
    super(props);

    this.state = {
      nInstalled: 0,
    };

    this.modal = React.createRef();
  }

  get imageName() {
    return this.props.channel?.imageName || this.props.imageName;
  }

  onRefreshVersions = (versions) => {
    this.setState({ nInstalled: versions ? versions.length : 0 });
  };

  openManager = () => {
    this.modal.current.openModal();
  };

  render() {
    const { nInstalled } = this.state;
    let icon = null;
    if (this.props.icon) {
      icon = (
        <span key="icon" className="mr-1">
          <i className={this.props.icon} />
        </span>
      );
    }

    return (
      <>
        <Button onClick={this.openManager}>
          {icon}
          {this.props.title || this.imageName}
          {nInstalled ? (
            <Badge pill color="info" className="ml-1">
              {nInstalled}
            </Badge>
          ) : null}
        </Button>
        <DockerImageManager ref={this.modal} {...this.props} onRefresh={this.onRefreshVersions} />
      </>
    );
  }
}
