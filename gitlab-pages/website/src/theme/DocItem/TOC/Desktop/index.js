import { ThemeClassNames } from "@docusaurus/theme-common";
import { useDoc } from "@docusaurus/theme-common/internal";
import { useSyntax } from "@theme/Syntax/SyntaxContext";
import TOC from "@theme/TOC";
import React from "react";

export default function DocItemTOCDesktop() {
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
    <TOC
      toc={getFilteredTOC()}
      minHeadingLevel={frontMatter.toc_min_heading_level}
      maxHeadingLevel={frontMatter.toc_max_heading_level}
      className={ThemeClassNames.docs.docTocDesktop}
    />
  );
}
