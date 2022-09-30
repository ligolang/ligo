import React, { useContext } from "react";
import clsx from "clsx";
import {
  NavbarSecondaryMenuFiller,
  ThemeClassNames,
} from "@docusaurus/theme-common";
import { useNavbarMobileSidebar } from "@docusaurus/theme-common/internal";
import DocSidebarItems from "@theme/DocSidebarItems";
import styles from "./styles.module.css";
import SyntaxSwitch from "@theme/Syntax/SyntaxSwitch";
import SyntaxContext from "@theme/Syntax/SyntaxContext";

// eslint-disable-next-line react/function-component-definition
const DocSidebarMobileSecondaryMenu = ({ sidebar, path }) => {
  const mobileSidebar = useNavbarMobileSidebar();

  const { syntax, setSyntax } = useContext(SyntaxContext);

  return (
    <ul className={clsx(ThemeClassNames.docs.docSidebarMenu, "menu__list")}>
      <div className={styles.switchContainer}>
        Display syntax:{" "}
        <SyntaxSwitch syntax={syntax} onSyntaxChange={setSyntax} />
      </div>
      <DocSidebarItems
        items={sidebar}
        activePath={path}
        onItemClick={item => {
          // Mobile sidebar should only be closed if the category has a link
          if (item.type === "category" && item.href) {
            mobileSidebar.toggle();
          }
          if (item.type === "link") {
            mobileSidebar.toggle();
          }
        }}
        level={1}
      />
    </ul>
  );
};
function DocSidebarMobile(props) {
  return (
    <NavbarSecondaryMenuFiller
      component={DocSidebarMobileSecondaryMenu}
      props={props}
    />
  );
}
export default React.memo(DocSidebarMobile);
