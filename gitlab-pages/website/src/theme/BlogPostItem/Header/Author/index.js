import Link from "@docusaurus/Link";
import clsx from "clsx";
import React from "react";
import styles from "./styles.module.css";
function MaybeLink(props) {
  if (props.href) {
    return <Link {...props} />;
  }
  return <>{props.children}</>;
}
export default function BlogPostItemHeaderAuthor({ author, className }) {
  const { name, title, url, imageURL, email } = author;
  const link = url || (email && `mailto:${email}`) || undefined;
  return (
    <span className={clsx(styles["avatar"], "margin-bottom--sm", className)}>
      {imageURL && (
        <MaybeLink href={link} className="avatar__photo-link">
          <img className={styles["avatar__photo"]} src={imageURL} alt={name} />
        </MaybeLink>
      )}

      {name && (
        <span
          className="avatar__intro"
          itemProp="author"
          itemScope
          itemType="https://schema.org/Person"
        >
          <MaybeLink
            href={link}
            itemProp="url"
            className={styles["avatar__name-link"]}
            title={title || `Link to ${name} profile`}
          >
            <span itemProp="name">{name}</span>
          </MaybeLink>
        </span>
      )}
    </span>
  );
}
