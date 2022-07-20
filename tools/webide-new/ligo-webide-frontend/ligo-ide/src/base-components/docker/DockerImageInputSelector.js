import React, { PureComponent } from "react";

import { DropdownInput } from "~/base-components/ui-components";

import platform from "~/base-components/platform";

import DockerImageManager from "./DockerImageManager";

export default class DockerImageInputSelector extends PureComponent {
  constructor(props) {
    super(props);

    this.state = {
      loading: true,
      versions: [],
    };

    this.modal = React.createRef();
  }

  get imageName() {
    return this.props.channel?.imageName || this.props.imageName;
  }

  onRefreshVersions = (versions) => {
    if (!this.props.disableAutoSelection && !this.props.selected && versions.length) {
      this.props.onSelected(versions[0].Tag);
    }
    this.setState({
      loading: false,
      versions: versions.map((v) => ({ id: v.Tag, display: v.Name || v.Tag })),
    });
  };

  openManager = () => {
    this.modal.current.openModal();
  };

  badgeProps = (selected) => {
    if (this.state.loading) {
      return {
        badge: (
          <span key="loading">
            <i className="fas fa-spin fa-spinner mr-1" />
            Loading...
          </span>
        ),
      };
    }
    if (!selected) {
      if (this.state.versions.length) {
        return { badge: "not selected", badgeColor: "danger" };
      }
      return;
    }
    if (
      !this.state.versions.find((v) => v.id === selected) &&
      !this.props.extraOptions?.find((i) => i.id === selected)
    ) {
      return { badge: "not installed", badgeColor: "danger" };
    }
  };

  render() {
    const { versions } = this.state;

    let placeholder = `Select a version of ${this.props.noneName || this.imageName}`;
    if (!versions.length) {
      placeholder = `(No ${this.props.noneName || this.imageName} installed)`;
      versions.push({ id: "none", display: placeholder, disabled: true });
    }

    let group;
    if (platform.isDesktop && !this.props.noManager) {
      group = (
        <>
          <i className="fas fa-download mr-1" />
          Installed
        </>
      );
    } else {
      group = (
        <>
          <i className="fas fa-download mr-1" />
          Versions
        </>
      );
    }
    let options = [
      {
        group,
        children: this.state.versions,
      },
    ];
    if (platform.isDesktop && !this.props.noManager) {
      options.push({
        id: "manager",
        display: (
          <span key="manager">
            <i className="fas fa-cog mr-1" />
            {this.props.modalTitle}...
          </span>
        ),
        onClick: this.openManager,
      });
    }
    if (this.props.extraOptions) {
      options = [...this.props.extraOptions, ...options];
    }

    const {
      size,
      label,
      disableAutoSelection,
      bg,
      inputClassName,
      selected,
      onSelected,
      readOnly,
    } = this.props;
    return (
      <>
        <DropdownInput
          size={size}
          label={typeof label === "string" ? label : this.imageName}
          options={options}
          disableAutoSelection={disableAutoSelection}
          bg={bg}
          inputClassName={inputClassName}
          placeholder={placeholder}
          {...this.badgeProps(selected)}
          value={selected?.toString() || ""}
          onChange={onSelected}
          readOnly={readOnly}
        />
        <DockerImageManager ref={this.modal} {...this.props} onRefresh={this.onRefreshVersions} />
      </>
    );
  }
}
