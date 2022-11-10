import React, { useContext } from "react";
import clsx from 'clsx';
import {useNavbarSecondaryMenu} from '@docusaurus/theme-common/internal';
import SyntaxSwitch from "@theme/Syntax/SyntaxSwitch";
import SyntaxContext from "@theme/Syntax/SyntaxContext";
import styles from "./styles.module.css";

export default function NavbarMobileSidebarLayout({
  header,
  primaryMenu,
  secondaryMenu,
}) {
  const {shown: secondaryMenuShown} = useNavbarSecondaryMenu();
  const { syntax, setSyntax } = useContext(SyntaxContext);

  return (
    <div className="navbar-sidebar">
      {header}
      <div>
        <div className={styles.switchContainer}>
        Display syntax:{" "}
        <SyntaxSwitch syntax={syntax} onSyntaxChange={setSyntax} />
      </div>
      </div>
      <div
        className={clsx('navbar-sidebar__items', {
          'navbar-sidebar__items--show-secondary': secondaryMenuShown,
        })}>
        <div className="navbar-sidebar__item menu">{primaryMenu}</div>
        <div className="navbar-sidebar__item menu">{secondaryMenu}</div>
      </div>
    </div>
  );
}
