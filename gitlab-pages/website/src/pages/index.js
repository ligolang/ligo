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
  pascaligo: {
    comment: [
      /\(\*[\s\S]+?\*\)/,
      // /\{[\s\S]+?\}/,
      /\/\/.*/,
    ],
    string: {
      pattern: /(?:'(?:''|[^'\r\n])*'|#[&$%]?[a-f\d]+)+|\^[a-z]/i,
      greedy: true,
    },
    keyword: [
      {
        // Turbo Pascal
        pattern:
          /(^|[^&])\b(?:absolute|array|asm|begin|case|const|constructor|destructor|do|downto|else|end|file|for|function|goto|if|implementation|inherited|inline|interface|label|nil|object|of|operator|packed|procedure|program|record|reintroduce|repeat|self|set|string|then|to|type|unit|until|uses|var|while|with)\b/i,
        lookbehind: true,
      },
      {
        // Free Pascal
        pattern: /(^|[^&])\b(?:dispose|exit|false|new|true)\b/i,
        lookbehind: true,
      },
      {
        // Object Pascal
        pattern:
          /(^|[^&])\b(?:class|dispinterface|except|exports|finalization|finally|initialization|inline|library|on|out|packed|property|raise|resourcestring|threadvar|try)\b/i,
        lookbehind: true,
      },
      {
        // Modifiers
        pattern:
          /(^|[^&])\b(?:absolute|abstract|alias|assembler|bitpacked|break|cdecl|continue|cppdecl|cvar|default|deprecated|dynamic|enumerator|experimental|export|external|far|far16|forward|generic|helper|implements|index|interrupt|iochecks|local|message|name|near|nodefault|noreturn|nostackframe|oldfpccall|otherwise|overload|override|pascal|platform|private|protected|public|published|read|register|reintroduce|result|safecall|saveregisters|softfloat|specialize|static|stdcall|stored|strict|unaligned|unimplemented|varargs|virtual|write)\b/i,
        lookbehind: true,
      },
    ],
    number: [
      // Hexadecimal, octal and binary
      /(?:[&%]\d+|\$[a-f\d]+)/i,
      // Decimal
      /\b\d+(?:\.\d+)?(?:e[+-]?\d+)?/i,
    ],
    operator: [
      /\.\.|\*\*|:=|<[<=>]?|>[>=]?|[+\-*\/]=?|[@^=]/i,
      {
        pattern:
          /(^|[^&])\b(?:and|as|div|exclude|in|include|is|mod|not|or|shl|shr|xor)\b/,
        lookbehind: true,
      },
    ],
    punctuation: /\(\.|\.\)|[()\[\]:;,.]/,
  },
  reasonligo: {
    ...Prism.languages.reason,
    comment: [/(^|[^\\])\/\*[\s\S]*?\*\//, /\(\*[\s\S]*?\*\)/, /\/\/.*/],
  },
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
      title={`Hello from ${siteConfig.title}`}
      description="Description will go into a meta tag in <head />"
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
                    href="https://ide-v2-beta.ligolang.org/"
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
