import React, { Component } from "react";
import classnames from "classnames";

import { ButtonDropdown, DropdownToggle, DropdownItem } from "reactstrap";

import { withRouter } from "react-router";

class User extends Component {
  state = {
    isDropdownOpen: false,
    loaded: false,
  };

  constructor(props) {
    super(props);
  }

  componentDidMount() {
    const img = new Image();
    img.src = null; // this.props.profile.get('avatar')
    img.crossOrigin = true;
    img.onload = () => {
      this.setState({ loaded: true });
    };
  }

  onToggle = (event) => {
    event.preventDefault();
    this.setState({ isDropdownOpen: !this.state.isDropdownOpen });
  };

  renderDropdownMenus = (profile) => {
    const links = [
      {
        name: "Install",
        link: "https://ligolang.org/docs/intro/installation/",
        icon: "fa-tools",
        key: "install",
      },
      {
        name: "Docs",
        link: "https://ligolang.org/docs/intro/introduction/",
        icon: "fa-book",
        key: "docs",
      },
      {
        name: "Tutorials",
        link: "https://ligolang.org/docs/tutorials/getting-started/getting-started/",
        icon: "fa-graduation-cap",
        key: "tutorials",
      },
      {
        name: "Blog",
        link: "https://forum.tezosagora.org/tag/ligo",
        icon: "fa-comment",
        key: "blog",
      },
      {
        name: "Ask Questions",
        link: "https://ligolang.org/contact/",
        icon: "fa-question",
        key: "ask-questions",
      },
      {
        name: "Changelog",
        link: "https://ligolang.org/docs/next/intro/changelog/",
        icon: "fa-list-ul",
        key: "changelog",
      },
      {
        name: "Cheat Sheet",
        link: "https://ligolang.org/docs/api/cheat-sheet",
        icon: "fa-hand-holding",
        key: "cheat-sheet",
      },
    ];

    const dropdownItems = [
      <DropdownItem key="divider-2" divider />,
      <DropdownItem key="my-projects" onClick={() => this.props.history.push("/local")}>
        <i className="fas fa-th-list w-3 mr-2" />
        My Projects
      </DropdownItem>,
      ...links.map((link) => {
        return (
          <DropdownItem key={link.key} onClick={() => window.open(link.link, "_blank")}>
            <i className={`fas ${link.icon} w-3 mr-2`} />
            {link.name}
          </DropdownItem>
        );
      }),
    ];

    const { isDropdownOpen } = this.state;
    return (
      <div
        className={classnames("dropdown-menu dropdown-menu-right", isDropdownOpen && "show")}
        style={{ right: 3, top: 48, width: "fit-content" }}
      >
        {dropdownItems}
      </div>
    );
  };

  render() {
    const { profile } = this.props;

    return (
      <ButtonDropdown
        group={false}
        className="d-flex flex-1 w-100"
        isOpen={this.state.isDropdownOpen}
        toggle={this.onToggle}
        onClick={(event) => event.preventDefault()}
      >
        <DropdownToggle tag="div" className="nav-dropdown-toggle px-2">
          <div className="d-flex bg-secondary align-items-center justify-content-center user-avatar">
            {this.state.loaded && profile.get("avatar") ? (
              <img className="user-avatar" src={profile.get("avatar")} crossOrigin="true" />
            ) : (
              <span>
                <span className="fa fa-user-alt" />
              </span>
            )}
          </div>
        </DropdownToggle>
        {this.renderDropdownMenus(profile)}
      </ButtonDropdown>
    );
  }
}

export default withRouter(User);
