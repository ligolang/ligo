import { useBlogPost } from "@docusaurus/theme-common/internal";
import BlogPostItemHeaderAuthor from "@theme/BlogPostItem/Header/Author";
import clsx from "clsx";
import React from "react";
import styles from "./styles.module.css";
// Component responsible for the authors layout
export default function BlogPostItemHeaderAuthors({ className }) {
  const {
    metadata: { authors },
    assets,
  } = useBlogPost();
  const authorsCount = authors.length;
  if (authorsCount === 0) {
    return null;
  }
  const imageOnly = authors.every(({ name }) => !name);
  return (
    <span className={clsx(styles["authors"], className)}>
      {authors.map((author, idx) => (
        <BlogPostItemHeaderAuthor
          key={idx}
          author={{
            ...author,
            // Handle author images using relative paths
            imageURL: assets.authorsImageUrls[idx] ?? author.imageURL,
          }}
        />
      ))}
    </span>
  );
}
