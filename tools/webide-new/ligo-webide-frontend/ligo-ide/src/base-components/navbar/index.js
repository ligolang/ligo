import React, { PureComponent } from "react";

import { NavLink } from "react-router-dom";
import { Navbar, Nav, UncontrolledTooltip } from "~/base-components/ui-components";
import { KeypairButton } from "~/base-components/keypair";

import NavLinkLeft from "./NavLinkLeft";
import NavLinkRight from "./NavLinkRight";
import "./styles.scss";

export default class Header extends PureComponent {
  renderLeftNavbar = (links) => {
    return links.map((link) => (
      <NavLinkLeft
        key={`nav-link-${link.route}`}
        route={link.route}
        title={link.title}
        icon={link.icon}
        contextMenu={link.contextMenu}
        selected={link.selected}
        dropdown={link.dropdown}
        onClickItem={link.onClickItem}
      />
    ));
  };

  renderRightNavbar = (links) => {
    return links.map((link) => (
      <NavLinkRight
        key={`nav-link-${link.route}`}
        route={link.route}
        title={link.title}
        icon={link.icon}
        noneIcon={link.noneIcon}
        selected={link.selected}
        dropdown={link.dropdown}
        logoIcon={link.logoIcon}
        onClickItem={link.onClickItem}
      />
    ));
  };

  renderContextMenu = (links) => {
    return links
      .map((link) => {
        return link.dropdown.map((item, index) => {
          if (!item.id || !link.contextMenu) {
            return null;
          }
          const contextMenu = link.contextMenu(item.id);
          if (!contextMenu) {
            return null;
          }
          return (
            <NavDropdownContextMenu
              key={`nav-context-menu-${link.route}-${item.id}-${index}`}
              route={link.route}
              item={item}
              contextMenu={contextMenu}
            />
          );
        });
      })
      .flat()
      .filter(Boolean);
  };

  render() {
    const {
      profile,
      navbarLeft,
      navbarRight,
      extraLoggedInOptions,
      children,
      onCancelKp,
      isOpenKeypair,
    } = this.props;
    const username = "local";

    return (
      <Navbar tag="header" dark expand>
        <Nav navbar className="navbar-left">
          {this.renderLeftNavbar(navbarLeft)}
        </Nav>
        {children}
        <Nav navbar className="ml-auto navbar-nav-scroll navbar-right">
          {this.renderRightNavbar(navbarRight)}
          <KeypairButton isOpenKeypair={isOpenKeypair} onCancel={onCancelKp}>
            <div className="btn btn-primary btn-flat px-3 py-75" id="keypair-manager">
              <i className="fas fa-key" />
            </div>
            <UncontrolledTooltip placement="bottom" target="keypair-manager">
              Keypair Manager
            </UncontrolledTooltip>
          </KeypairButton>
        </Nav>
      </Navbar>
    );
  }
}
