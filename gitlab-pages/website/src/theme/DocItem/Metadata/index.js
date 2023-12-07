import { PageMetadata } from "@docusaurus/theme-common";
import { useDoc } from "@docusaurus/theme-common/internal";
import { useSyntax } from "@theme/Syntax/SyntaxContext";
import React from "react";
export default function DocItemMetadata() {
  const { metadata, frontMatter, assets } = useDoc();
  const { syntax } = useSyntax();

  return (
    <PageMetadata
      title={
        frontMatter.jsLigoTitle && syntax === "jsligo" ? frontMatter.jsLigoTitle : metadata.title
      }
      description={metadata.description}
      keywords={frontMatter.keywords}
      image={assets.image ?? frontMatter.image}
    />
  );
}
