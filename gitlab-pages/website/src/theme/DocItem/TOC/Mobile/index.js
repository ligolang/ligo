import { ThemeClassNames } from "@docusaurus/theme-common";
import { useDoc } from "@docusaurus/theme-common/internal";
import { useSyntax } from "@theme/Syntax/SyntaxContext";
import TOCCollapsible from "@theme/TOCCollapsible";
import clsx from "clsx";
import React from "react";
import styles from "./styles.module.css";

export default function DocItemTOCMobile() {
  const { toc, frontMatter } = useDoc();
  const { syntax } = useSyntax();

  const getFilteredTOC = React.useCallback(() => {
    if (typeof window === "undefined") return toc;

    return (toc || []).filter((item) => {
      if (item.id && document.getElementById(item.id)) {
        return true;
      }
      return false;
    });
  }, [syntax]);

  return (
    <TOCCollapsible
      toc={getFilteredTOC()}
      minHeadingLevel={frontMatter.toc_min_heading_level}
      maxHeadingLevel={frontMatter.toc_max_heading_level}
      className={clsx(ThemeClassNames.docs.docTocMobile, styles.tocMobile)}
    />
  );
}
