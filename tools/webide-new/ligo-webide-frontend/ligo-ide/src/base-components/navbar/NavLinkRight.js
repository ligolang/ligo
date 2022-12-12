import React, { PureComponent } from "react";

import { withRouter } from "react-router";
import { NavLink } from "react-router-dom";

import NavLinkContent from "./NavLinkContent";
import NavDropdown from "./NavDropdown";

class NavLinkRight extends PureComponent {
  static defaultProps = {
    onClickItem: () => {},
  };

  onClickItem = (item) => {
    const { route, history } = this.props;
    this.props.onClickItem(item.id, item);
    if (history.location.pathname.startsWith(`/${route}`) && item.id !== "custom") {
      history.push(`/${route}/${item.id}`);
    }
  };

  render() {
    const { route, title, selected, dropdown, icon, noneIcon, logoIcon } = this.props;

    return (
      <NavLink
        to={`/${route}/${selected.id || ""}`}
        className="nav-link d-flex p-0"
        activeClassName="active"
      >
        <NavLinkContent
          title={title}
          selected={selected.name}
          icon={icon}
          logoIcon={selected.logoIcon}
          id={selected.id}
          noneIcon={noneIcon}
          width="5.9rem"
        />
        <NavDropdown
          right
          route={route}
          selected={selected.id || ""}
          list={dropdown}
          onClickItem={this.onClickItem}
          icon={icon}
          logoIcon={logoIcon}
        />
      </NavLink>
    );
  }
}

export default withRouter(NavLinkRight);
