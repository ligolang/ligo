import LinkAsButton from "@site/src/components/Buttons/link-as-button";
import React from "react";
import styles from "./styles.module.css";

type CommunityProps = {
  discordMembers: number;
  contributors?: number;
  packages?: number;
};

const Community = ({ discordMembers, contributors = 78, packages }: CommunityProps) => {
  const communityItem = [
    {
      id: "contributors",
      value: contributors,
      title: "contributors",
      description:
        "Ligo is open-source and open for contributions. Join the Ligo team on GitLab and start contributing!",
      cta: {
        label: "contribute on gitlab",
        href: "https://gitlab.com/ligolang/ligo",
        rel: "noopener noreferrer nofollow",
      },
    },
    {
      id: "discord-members",
      value: discordMembers,
      title: "members on discord",
      description:
        "The Ligo community is growing fast. Join our Discord to talk about the Tezos ecosystem, find help, and discover a lot more.",
      cta: {
        label: "join our discord",
        href: "https://discord.gg/tezos",
        rel: "noopener noreferrer nofollow",
      },
    },
    {
      id: "registry-projects",
      value: packages,
      title: "packages",
      description:
        "The Ligo team and community have exposed some packages that you can reuse when developing your own contracts. Discover them on our registry!",
      cta: {
        label: "see our registry",
        href: "https://packages.ligolang.org/",
        rel: "",
      },
    },
  ];

  return (
    <div className={styles["community"]}>
      {communityItem.map((item) => (
        <article className={styles["community__article"]} key={item.id}>
          <p id={item.id} className={styles["community__article-kpi"]}>
            {item.value}
          </p>
          <h3 className={styles["community__article-title"]}>{item.title}</h3>
          <p className={styles["community__article-description"]}>{item.description}</p>
          <LinkAsButton variant="ghost" href={item.cta.href} rel={item.cta.rel}>
            {item.cta.label}
          </LinkAsButton>
        </article>
      ))}
    </div>
  );
};

export default Community;
