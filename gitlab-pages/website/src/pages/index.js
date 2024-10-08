import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Hero from "@site/src/components/Hero";
import ThemeReset from "@site/src/components/ThemeReset";
import Layout from "@theme/Layout";
import React, { useEffect } from "react";
import Community from "../components/Community";
import HomeSection from "../components/HomeSection";
import HomepageCodeExamples from "../components/HomepageCodeExamples";
import HomepageFeatures from "../components/HomepageFeatures";
import HomepagePartners from "../components/HomepagePartners";
import News from "../components/News";
import useDiscordMembers from "../hooks/useDiscordMembers";
import useElementOnScreen from "../hooks/useElementOnScreen";
import usePackages from "../hooks/usePackages";
import styles from "./index.module.scss";
const { Prism } = require("prism-react-renderer");
Prism.languages = {
  ...Prism.languages,
  cameligo: {
    ...Prism.languages.ocaml,
    comment: [/(^|[^\\])\/\*[\s\S]*?\*\//, /\(\*[\s\S]*?\*\)/, /\/\/.*/],
  },
  jsligo: Prism.languages.typescript,
};

function animateValue(obj, start, end, duration) {
  let startTimestamp = null;
  const step = (timestamp) => {
    if (!startTimestamp) startTimestamp = timestamp;
    const progress = Math.min((timestamp - startTimestamp) / duration, 1);
    obj.innerHTML = Math.floor(progress * (end - start) + start);
    if (progress < 1) {
      window.requestAnimationFrame(step);
    }
  };
  window.requestAnimationFrame(step);
}

function reveal() {
  var reveals = document.querySelectorAll(".reveal");
  for (var i = 0; i < reveals.length; i++) {
    var windowHeight = window.innerHeight;
    var elementTop = reveals[i].getBoundingClientRect().top;
    var elementVisible = 150;
    if (elementTop < windowHeight - elementVisible) {
      reveals[i].classList.add("active");
    } else {
      reveals[i].classList.remove("active");
    }
  }
}

export default function Home() {
  const { siteConfig } = useDocusaurusContext();
  const discordMembers = useDiscordMembers();
  const packages = usePackages();
  const [containerRef, isVisible] = useElementOnScreen({
    rootMargin: "0px",
    threshold: 0.6,
  });

  useEffect(() => {
    window.addEventListener("scroll", reveal);
    reveal();
    return () => {
      window.removeEventListener("scroll", reveal);
    };
  }, []);

  useEffect(() => {
    if (isVisible) {
      const contributors = document.getElementById("contributors");
      const members = document.getElementById("discord-members");
      const projects = document.getElementById("registry-projects");

      animateValue(contributors, 0, 78, 1000);
      animateValue(members, 0, discordMembers, 1000);
      animateValue(projects, 0, packages, 1000);
    }
  }, [isVisible, discordMembers, packages]);

  return (
    <Layout
      title="Ligo, the smart-contract language for Tezos"
      description="Ligo is a simple smart-contract language built for Tezos, made for developers."
    >
      <ThemeReset />
      <Hero />
      <main className={styles.home__main}>
        <div className={styles.container}>
          <HomeSection
            id="multi-syntax"
            title={
              <span>
                Ligo Loves <span className={styles["section__title-highlighted"]}>Everyone</span>
              </span>
            }
            subtitle={"So, we built it with multi-syntax."}
          >
            <HomepageCodeExamples />
          </HomeSection>

          <HomeSection
            id="features"
            title={
              <span>
                <span className={styles["section__title-highlighted"]}>Build it</span> with Ligo
              </span>
            }
            subtitle={"Discover everything you need to safely interact with the Tezos blockchain."}
          >
            <HomepageFeatures />
          </HomeSection>
          <HomeSection
            id="news"
            title={
              <span>
                What's New
                <span className={styles["section__title-highlighted"]}> in Ligo </span> ?
              </span>
            }
            subtitle={"We're constantly evolving. Keep in touch."}
          >
            <News />
          </HomeSection>
        </div>
        <HomeSection
          id="community"
          ref={containerRef}
          title={
            <span>
              Build Ligo
              <span className={styles["section__title-highlighted"]}> Together </span>
            </span>
          }
          subtitle={"Developers all over the world are building the future of Ligo together."}
        >
          <Community discordMembers={discordMembers} packages={packages} />
        </HomeSection>
        <div className={styles.container}>
          <HomeSection
            id="open-sources"
            title={"Open Sources"}
            subtitle={"Made possible by our awesome partners."}
          >
            <HomepagePartners />
          </HomeSection>
        </div>
      </main>
    </Layout>
  );
}
