import useBaseUrl from "@docusaurus/useBaseUrl";
import LinkAsButton from "@site/src/components/Buttons/link-as-button";
import Batcher from "@site/src/components/Icons/Batcher";
import Crunchy from "@site/src/components/Icons/Crunchy";
import FocusedArt from "@site/src/components/Icons/FocusedArt";
import PlayMakers from "@site/src/components/Icons/PlayMakers";
import Plenty from "@site/src/components/Icons/Plenty";
import Route3 from "@site/src/components/Icons/Route3";
import SlicedArt from "@site/src/components/Icons/SlicedArt";
import Smartchain from "@site/src/components/Icons/Smartchain";
import Starlords from "@site/src/components/Icons/Starlords";
import TZSafe from "@site/src/components/Icons/TZSafe";
import TZVote from "@site/src/components/Icons/TZVote";
import TezosDomains from "@site/src/components/Icons/TezosDomains";
import Title, { Subtitle } from "@site/src/components/Titles";
import React from "react";
import styles from "./styles.module.scss";

const brands = [
  <a href="https://plentydefi.com/" rel="noopener noreferrer nofollow" target="_blank">
    <Plenty height={50} />
  </a>,
  <a href="https://sliced.art/" rel="noopener noreferrer nofollow" target="_blank">
    <SlicedArt height={50} />
  </a>,
  <a href="https://tzsafe.marigold.dev/" rel="noopener noreferrer nofollow" target="_blank">
    <TZSafe height={70} />
  </a>,
  <a href="https://tzvote.marigold.dev/" rel="noopener noreferrer nofollow" target="_blank">
    <TZVote height={60} />
  </a>,
  <a href="https://batcher.marigold.dev/" rel="noopener noreferrer nofollow" target="_blank">
    <Batcher height={50} />
  </a>,
  <a href="https://focused.art/" rel="noopener noreferrer nofollow" target="_blank">
    <FocusedArt height={40} />
  </a>,
  <a href="https://crunchy.network/" rel="noopener noreferrer nofollow" target="_blank">
    <Crunchy height={40} />
  </a>,
  <a href="https://starlords.xyz/" rel="noopener noreferrer nofollow" target="_blank">
    <Starlords height={50} />
  </a>,
  <a href="https://tezos.domains" rel="noopener noreferrer nofollow" target="_blank">
    <TezosDomains height={50} />
  </a>,
  <a href="https://www.smart-chain.fr/" rel="noopener noreferrer nofollow" target="_blank">
    <Smartchain height={50} />
  </a>,
  <a href="https://3route.io/" rel="noopener noreferrer nofollow" target="_blank">
    <Route3 height={40} />
  </a>,
  <a href="https://www.playmakers.co/" rel="noopener noreferrer nofollow" target="_blank">
    <PlayMakers height={50} />
  </a>,
];

const Hero = () => (
  <section className={styles.hero}>
    <Title level={1}>
      <span>Discover Ligo</span>
      <br />
      <span className={styles["hero__title-small"]}>smart contracts made easy</span>
    </Title>
    <Subtitle>
      A simple smart-contract language <br /> built for <b>Tezos</b>, made for <b>developers</b>.
    </Subtitle>
    <div className={styles["hero__cta"]}>
      <LinkAsButton aria-label="Get started" href={useBaseUrl("docs/intro/introduction")}>
        Get started
      </LinkAsButton>
      <LinkAsButton
        href="https://ide.ligolang.org/"
        aria-label="Try Ligo online"
        title="Go to our Web-IDE to try Ligo online"
        target="_blank"
        rel="external"
        variant="ghost"
      >
        Try Ligo online
      </LinkAsButton>
    </div>
    <div className={styles["hero__best-users"]}>
      <div className={styles["hero__marquee"]}>
        <ul className={styles["hero__marquee-content"]}>
          {brands.map((brand, index) => (
            <li key={`brand-${index}`}>{brand}</li>
          ))}
        </ul>
        <ul className={styles["hero__marquee-content"]} aria-hidden="true">
          {brands.map((brand, index) => (
            <li key={`marquee-brand-${index}`}>{brand}</li>
          ))}
        </ul>
      </div>
    </div>
  </section>
);

export default Hero;
