import { translate } from "@docusaurus/Translate";
import { usePluralForm } from "@docusaurus/theme-common";
import { useBlogPost } from "@docusaurus/theme-common/internal";
import Tag from "@theme/Tag";
import clsx from "clsx";
import React from "react";
import BlogPostItemHeaderAuthors from "../Authors";
import styles from "./styles.module.css";

// Very simple pluralization: probably good enough for now
function useReadingTimePlural() {
  const { selectMessage } = usePluralForm();
  return (readingTimeFloat) => {
    const readingTime = Math.ceil(readingTimeFloat);
    return selectMessage(
      readingTime,
      translate(
        {
          id: "theme.blog.post.readingTime.plurals",
          description:
            'Pluralized label for "{readingTime} min read". Use as much plural forms (separated by "|") as your language support (see https://www.unicode.org/cldr/cldr-aux/charts/34/supplemental/language_plural_rules.html)',
          message: "One min read|{readingTime} min read",
        },
        { readingTime }
      )
    );
  };
}
function ReadingTime({ readingTime }) {
  const readingTimePlural = useReadingTimePlural();
  return <>{readingTimePlural(readingTime)}</>;
}
function Date({ date, formattedDate }) {
  return (
    <time dateTime={date} itemProp="datePublished">
      {formattedDate}
    </time>
  );
}
function Spacer() {
  return <span className={styles["spacer"]}>·</span>;
}
export default function BlogPostItemHeaderInfo({ className }) {
  const { metadata } = useBlogPost();
  const { date, formattedDate, readingTime, authors, tags } = metadata;
  const tagsExists = tags.length > 0;

  return (
    <div className={clsx(styles.container, "margin-vert--md", className)}>
      <p className={styles["blog-post-header__info"]}>
        {authors.length > 0 && (
          <>
            <BlogPostItemHeaderAuthors authors={authors} />
            <Spacer />
          </>
        )}
        {typeof readingTime !== "undefined" && (
          <>
            <ReadingTime readingTime={readingTime} />
            <Spacer />
          </>
        )}
        <Date date={date} formattedDate={formattedDate} />
      </p>

      {tagsExists && (
        <ul className={styles.tags}>
          {tags.map(({ label, permalink: tagPermalink }) => (
            <li key={tagPermalink} className={styles.tag}>
              <Tag label={label} permalink={tagPermalink} />
            </li>
          ))}
        </ul>
      )}
    </div>
  );
}
