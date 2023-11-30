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
import Marquee from "react-fast-marquee";
import styles from "./styles.module.scss";

const BRANDS = [
  { link: "https://plentydefi.com/", component: <Plenty height={50} /> },
  { link: "https://sliced.art/", component: <SlicedArt height={50} /> },
  { link: "https://tzsafe.marigold.dev/", component: <TZSafe height={70} /> },
  { link: "https://tzvote.marigold.dev/", component: <TZVote height={60} /> },
  { link: "https://batcher.marigold.dev/", component: <Batcher height={50} /> },
  { link: "https://focused.art/", component: <FocusedArt height={40} /> },
  { link: "https://crunchy.network/", component: <Crunchy height={40} /> },
  { link: "https://starlords.xyz/", component: <Starlords height={50} /> },
  { link: "https://tezos.domains", component: <TezosDomains height={50} /> },
  { link: "https://www.smart-chain.fr/", component: <Smartchain height={50} /> },
  { link: "https://3route.io/", component: <Route3 height={40} /> },
  { link: "https://www.playmakers.co/", component: <PlayMakers height={50} /> },
].sort(() => 0.5 - Math.random());

const SPEED_PIXELS_PER_SECONDS = BRANDS.length * 3;

const Hero = () => {
  return (
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
        <Marquee pauseOnHover speed={SPEED_PIXELS_PER_SECONDS} className={styles["hero__marquee"]}>
          {BRANDS.map((user) => (
            <a
              key={user.link}
              href={user.link}
              rel="noopener noreferrer nofollow"
              target="_blank"
              className={styles["hero__marquee-content"]}
            >
              {user.component}
            </a>
          ))}
        </Marquee>
      </div>
    </section>
  );
};

export default Hero;
