import useBaseUrl from "@docusaurus/useBaseUrl";
import Image from "@site/src/components/Image";
import Title, { Subtitle } from "@site/src/components/Titles";
import clsx from "clsx";
import React from "react";
import styles from "./index.module.scss";

const COMMUNICATION_CHANNELS = [
  {
    link: "https://discord.gg/tezos",
    icon: {
      src: "img/communication_channels/discord.svg",
      width: "30",
      height: "30",
      alt: "",
    },
    description: "Join the Ligo Discord server. We're here to help.",
  },
  {
    link: "https://www.youtube.com/playlist?list=PLmDOlM4rqLvInuervAU4YuU39ThuHw3fM",
    icon: {
      src: "img/communication_channels/youtube.svg",
      width: "33",
      height: "26",
      alt: "",
    },
    description: "See Ligo latest videos on YouTube",
  },
  {
    link: "https://gitlab.com/ligolang/ligo/issues",
    icon: {
      src: "img/communication_channels/gitlab.svg",
      width: "30",
      height: "30",
      alt: "",
    },
    description: "Need a fix? Create an issue on the Ligo GitLab.",
  },
  {
    link: "https://x.com/ligolang",
    icon: {
      src: "img/communication_channels/x.svg",
      width: "26",
      height: "26",
      alt: "",
    },
    description: "Follow Ligo on X / Twitter to get news.",
  },
  {
    link: "https://t.me/LigoLang",
    icon: {
      src: "img/communication_channels/telegram.svg",
      width: "30",
      height: "30",
      alt: "",
    },
    description: "Talk with Ligo team on Telegram",
  },
  {
    link: "https://www.linkedin.com/company/marigold-ligolang/",
    icon: {
      src: "img/communication_channels/linkedin.svg",
      width: "30",
      height: "30",
      alt: "",
    },
    description: "Discover Ligo on LinkedIn",
  },
];

export default function FooterLayout({ style, links, logo, copyright }) {
  return (
    <footer>
      <div className={clsx("footer")}>
        <div className={clsx("container", styles["footer__content"])}>
          <div className={styles["footer__brand"]}>
            {(logo || copyright) && (
              <div className="footer__bottom">
                {logo && <div className="margin-bottom--sm">{logo}</div>}
              </div>
            )}
            <Title level={3}>Ligo</Title>
            <Subtitle className={styles["footer__brand-subtitle"]}>
              The smart-contract language tailored for Tezos.
            </Subtitle>
          </div>
          <div className={styles["footer__links"]}>{links}</div>
        </div>
      </div>
      <div className={styles["footer__after"]}>
        <div className={clsx("container", styles["footer__after-content"])}>
          {copyright}
          <ul className={styles["footer__socials"]}>
            {COMMUNICATION_CHANNELS.map((social) => (
              <li key={social.link} className={styles["footer__socials-item"]}>
                <a href={social.link} target="_blank" rel="noopener noreferrer nofollow">
                  <Image
                    src={useBaseUrl(social.icon.src)}
                    height={social.icon.height}
                    width={social.icon.width}
                    alt={social.icon.alt}
                  />
                  <span className="sr-only">{social.description}</span>
                </a>
              </li>
            ))}
          </ul>
        </div>
      </div>
    </footer>
  );
}
