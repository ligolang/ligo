import useBaseUrl from "@docusaurus/useBaseUrl";
import LinkAsButton from "@site/src/components/Buttons/link-as-button";
import Image from "@site/src/components/Image";
import Title from "@site/src/components/Titles";
import clsx from "clsx";
import React, { ReactNode } from "react";
import styles from "./styles.module.scss";

type Size = "small" | "large";
type NewsItem = {
  title: ReactNode;
  cta: ReactNode;
  description: string;
  size: Size;
  href: string;
};

const NewsItem = ({ title, description, size, cta, href }: NewsItem) => (
  <div
    className={clsx(
      styles["news-item"],
      size === "large" ? styles["news-item--large"] : styles["news-item--small"]
    )}
  >
    <div className={`${styles["news-item__content"]}`}>
      <Title level={3} className={styles["news-item__content-title"]}>
        {title}
      </Title>
      <p className={styles["news-item__content-description"]}>{description}</p>
      <LinkAsButton aria-label={`Link to ${href}`} type="button" variant="ghost" href={href}>
        {cta}
      </LinkAsButton>
    </div>
  </div>
);

const News = () => {
  const news: NewsItem[] = [
    {
      title: (
        <>
          Follow-us
          <br />
          on X
        </>
      ),
      description: "Ligo team shares news on X (Twitter); don't miss them and follow us.",
      size: "small",
      href: "https://x.com/LigoLang",
      cta: (
        <>
          Follow-us on{" "}
          <Image
            src={useBaseUrl("img/communication_channels/x-white.svg")}
            height={15}
            width={15}
            alt={"Follow Ligo on X"}
          />
        </>
      ),
    },
    {
      title: (
        <>
          Take a look <br />
          at the Changelog
        </>
      ),
      description:
        "The Ligo team and the community are improving the smart-contract language every day. See the recent changes on our up-to-date Changelog.",
      size: "large",
      href: useBaseUrl("docs/intro/changelog"),

      cta: "Go to Ligo Changelog",
    },
    {
      title: "Events",
      description:
        "The Ligo team is contributing too. See us at conferences to discover Ligo through a talk given by one of our teammates.",
      size: "large",
      href: "https://tezos.com/tez-dev",

      cta: "ligo at tez/dev",
    },
    {
      title: "GitLab",
      description:
        "Ligo sources are available on our GitLab. Feel free to open an issue or contribute.",
      size: "small",
      href: "https://gitlab.com/ligolang/ligo",

      cta: "Contribute to Ligo",
    },
  ];
  return (
    <div className={`${styles.home__news}`}>
      {news.map((item) => (
        <NewsItem key={item.href} {...item} />
      ))}
    </div>
  );
};

export default News;
