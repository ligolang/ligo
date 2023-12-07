import useBaseUrl from "@docusaurus/useBaseUrl";
import React from "react";
import Image from "../Image";
import styles from "./styles.module.scss";

const FEATURES = [
  {
    image: "img/ligo_features/ligo-feature-static-analysis.svg",
    title: "Static Analysis",
    content: "Ligo use static analysis and strong typing to help you write safer code.",
    link: "docs/language-basics/types",
  },
  {
    image: "img/ligo_features/ligo-feature-multi-syntax.svg",
    title: "Multi-syntax",
    content:
      "Ligo brings syntaxes that fit your preferences: TypeScript-inspired or OCaml-inspired.",
    link: "docs/intro/introduction#ligo-for-newcomers-or-confirmed-developpers",
  },
  {
    image: "img/ligo_features/ligo-feature-optimized.svg",
    title: "Optimized by Design",
    content: "Ligo is a high-level language that compiles to optimized Tezos bytecode.",
    link: "docs/intro/introduction#ligo-designed-to-be-cost-effective",
  },
  {
    image: "img/ligo_features/ligo-feature-testing-system.svg",
    title: "Testing System",
    content:
      "Ligo uses a robust testing system to simulate the Tezos blockchain, as if you were inside.",
    link: "docs/advanced/testing",
  },
  {
    image: "img/ligo_features/ligo-feature-community.svg",
    title: "Community",
    content:
      "Ligo's community is here to help. Join us on Discord, or let's learn by doing on our Registry.",
    link: "#community",
  },
  {
    image: "img/ligo_features/ligo-feature-tooling.svg",
    title: "First Class Tooling",
    content:
      "Ligo cares about Developer Experience, so we've developed a set of tools made just for you.",
    link: "docs/intro/introduction#a-set-of-tools-already-available",
  },
];

const Feature = (props) => (
  <a
    className={styles.feature}
    key={props.title}
    href={props.link}
    title={`Ligo ${props.title}`}
    aria-label={`Ligo ${props.title}`}
  >
    <Image className={styles.feature__icon} src={useBaseUrl(props.image)} alt="" />

    <h3 className={styles.feature__title}>{props.title}</h3>
    <p className={styles.feature__description}>{props.content}</p>
  </a>
);

export default function HomepageFeatures() {
  return (
    <div id="features" className={`${styles.features}`}>
      {FEATURES.map((entry) => (
        <Feature
          key={entry.title}
          title={entry.title}
          content={entry.content}
          image={entry.image}
          link={entry.link}
        />
      ))}
    </div>
  );
}
