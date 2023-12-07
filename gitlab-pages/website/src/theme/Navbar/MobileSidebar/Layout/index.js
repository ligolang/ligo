import { useNavbarSecondaryMenu } from "@docusaurus/theme-common/internal";
import Title from "@site/src/components/Titles";
import { useSyntax } from "@theme/Syntax/SyntaxContext";
import SyntaxSwitch from "@theme/Syntax/SyntaxSwitch";
import clsx from "clsx";
import React from "react";

export default function NavbarMobileSidebarLayout({ header, primaryMenu, secondaryMenu }) {
  const { shown: secondaryMenuShown } = useNavbarSecondaryMenu();
  const { syntax, setSyntax } = useSyntax();
  return (
    <div className="navbar-sidebar">
      {header}
      <div>
        <div className="navbar-sidebar__switch-container">
          <Title level={5} className="navbar-sidebar__switch-container-title">
            Syntax Preference
          </Title>
          <SyntaxSwitch syntax={syntax} onSyntaxChange={setSyntax} />
        </div>
      </div>
      <div
        className={clsx("navbar-sidebar__items", {
          "navbar-sidebar__items--show-secondary": secondaryMenuShown,
        })}
      >
        <div className="navbar-sidebar__item menu">{primaryMenu}</div>
        <div className="navbar-sidebar__item menu">{secondaryMenu}</div>
      </div>
    </div>
  );
}
