import { useBlogPost } from "@docusaurus/theme-common/internal";
import ReadMoreLink from "@theme/BlogPostItem/Footer/ReadMoreLink";
import EditThisPage from "@theme/EditThisPage";
import clsx from "clsx";
import React from "react";
import styles from "./styles.module.css";
export default function BlogPostItemFooter() {
  const { metadata, isBlogPostPage } = useBlogPost();
  const { title, editUrl, hasTruncateMarker } = metadata;
  // A post is truncated if it's in the "list view" and it has a truncate marker
  const truncatedPost = !isBlogPostPage && hasTruncateMarker;
  const renderFooter = truncatedPost || editUrl;
  if (!renderFooter) {
    return null;
  }
  return (
    <footer className={clsx("margin-top--sm", isBlogPostPage && styles.blogPostFooterDetailsFull)}>
      {isBlogPostPage && editUrl && (
        <div className="col margin-top--sm">
          <EditThisPage editUrl={editUrl} />
        </div>
      )}

      {truncatedPost && <ReadMoreLink blogPostTitle={title} to={metadata.permalink} />}
    </footer>
  );
}
