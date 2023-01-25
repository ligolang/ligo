import React from "react";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Layout from "@theme/Layout";
import HomepagePartners from "@site/src/components/HomepagePartners";
import HomepageFeatures from "../components/HomepageFeatures";
import HomepageCodeExamples from "../components/HomepageCodeExamples";

import useBaseUrl from "@docusaurus/useBaseUrl";

const { Prism } = require("prism-react-renderer");

Prism.languages = {
  ...Prism.languages,
  cameligo: {
    ...Prism.languages.ocaml,
    comment: [/(^|[^\\])\/\*[\s\S]*?\*\//, /\(\*[\s\S]*?\*\)/, /\/\/.*/],
  },
  jsligo: Prism.languages.typescript,
};

export default function Home() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout
      title={`${siteConfig.title}`}
      description="A friendly Smart Contract Language for Tezos"
    >
      <main>
        <div
          id="homePage"
          style={{
            display: "flex",
            justifyContent: "stretch",
            alignItems: "stretch",
            fontSize: "20px",
            flexDirection: "column",
          }}
        >
          <div id="intro" className="centered">
            <div id="callToAction">
              <ul>
                <li className="primary">
                  <a href="https://ide.ligolang.org">Try Online</a>
                </li>
                <li className="primary">
                  <a
                    href="https://ligo-webide-v2.gcp.marigold.dev"
                    target="_blank"
                  >
                    WebIde V2 beta
                  </a>
                </li>
                <li className="primary">
                  <a
                    href="https://gitpod.io/#https://gitlab.com/ligolang/template-ligo"
                    target="_blank"
                  >
                    Try on Gitpod
                  </a>
                </li>
                <li className="secondary">
                  <a href={useBaseUrl("/docs/intro/installation")}>Install</a>
                </li>
                <li className="secondary">
                  <a href={useBaseUrl("https://academy.ligolang.org/")}>
                    Learn on Academy
                  </a>
                </li>
              </ul>
            </div>
            <HomepageCodeExamples />
          </div>
          <HomepageFeatures />
          <HomepagePartners />
        </div>
      </main>
    </Layout>
  );
}
