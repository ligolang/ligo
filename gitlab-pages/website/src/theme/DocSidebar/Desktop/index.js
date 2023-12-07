import { useThemeConfig } from "@docusaurus/theme-common";
import Title from "@site/src/components/Titles";
import CollapseButton from "@theme/DocSidebar/Desktop/CollapseButton";
import Content from "@theme/DocSidebar/Desktop/Content";
import Logo from "@theme/Logo";
import { useSyntax } from "@theme/Syntax/SyntaxContext";
import SyntaxSwitch from "@theme/Syntax/SyntaxSwitch";
import clsx from "clsx";
import React from "react";
import styles from "./styles.module.css";
function DocSidebarDesktop({ path, sidebar, onCollapse, isHidden }) {
  const {
    navbar: { hideOnScroll },
    docs: {
      sidebar: { hideable },
    },
  } = useThemeConfig();

  const { syntax, setSyntax } = useSyntax();

  return (
    <div
      className={clsx(
        styles.sidebar,
        hideOnScroll && styles.sidebarWithHideableNavbar,
        isHidden && styles.sidebarHidden
      )}
    >
      {hideOnScroll && <Logo tabIndex={-1} className={styles.sidebarLogo} />}
      <div className="navbar-sidebar__switch-container">
        <Title level={5} className="navbar-sidebar__switch-container-title">
          Syntax Preference
        </Title>
        <SyntaxSwitch syntax={syntax} onSyntaxChange={setSyntax} />
      </div>
      <Content path={path} sidebar={sidebar} />
      {hideable && <CollapseButton onClick={onCollapse} />}
    </div>
  );
}
export default React.memo(DocSidebarDesktop);
