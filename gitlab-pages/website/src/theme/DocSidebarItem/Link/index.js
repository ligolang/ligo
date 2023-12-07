import Link from "@docusaurus/Link";
import isInternalUrl from "@docusaurus/isInternalUrl";
import { ThemeClassNames } from "@docusaurus/theme-common";
import { isActiveSidebarItem } from "@docusaurus/theme-common/internal";
import IconExternalLink from "@theme/Icon/ExternalLink";
import { useSyntax } from "@theme/Syntax/SyntaxContext";
import clsx from "clsx";
import React from "react";
import styles from "./styles.module.css";

export default function DocSidebarItemLink({
  item,
  onItemClick,
  activePath,
  level,
  index,
  ...props
}) {
  const { href, label, className, autoAddBaseUrl, customProps } = item;
  const isActive = isActiveSidebarItem(item, activePath);
  const isInternalLink = isInternalUrl(href);
  const { syntax } = useSyntax();
  return (
    <li
      className={clsx(
        ThemeClassNames.docs.docSidebarItemLink,
        ThemeClassNames.docs.docSidebarItemLinkLevel(level),
        "menu__list-item",
        className
      )}
      key={label}
    >
      <Link
        className={clsx("menu__link", !isInternalLink && styles.menuExternalLink, {
          "menu__link--active": isActive,
        })}
        autoAddBaseUrl={autoAddBaseUrl}
        aria-current={isActive ? "page" : undefined}
        to={href}
        {...(isInternalLink && {
          onClick: onItemClick ? () => onItemClick(item) : undefined,
        })}
        {...props}
      >
        {customProps && customProps.jsLigoName && syntax === "jsligo"
          ? customProps.jsLigoName
          : label}
        {!isInternalLink && <IconExternalLink />}
      </Link>
    </li>
  );
}
