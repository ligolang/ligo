import CAMELIGO_EXAMPLE from "!!raw-loader!./cameligo.mligo";
import JSLIGO_EXAMPLE from "!!raw-loader!./jsligo.jsligo";
import { useColorMode } from "@docusaurus/theme-common";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import TabItem from "@theme/TabItem";
import Tabs from "@theme/Tabs";
import clsx from "clsx";
import Highlight, { defaultProps } from "prism-react-renderer";
import defaultTheme from "prism-react-renderer/themes/palenight";
import React from "react";
import styles from "./styles.module.css";

function CodeExamples(props) {
  const {
    siteConfig: {
      themeConfig: { prism = {} },
    },
  } = useDocusaurusContext();
  const lightModeTheme = prism.theme || defaultTheme;
  const darkModeTheme = prism.darkTheme || lightModeTheme;
  const prismTheme = useColorMode().colorMode === "dark" ? darkModeTheme : lightModeTheme;

  return (
    <Tabs
      defaultValue="jsligo"
      // attributes={{ className: styles.tabPanel }}
      values={[
        { label: "JsLIGO", value: "jsligo" },
        { label: "CameLIGO", value: "cameligo" },
      ]}
    >
      <TabItem value="jsligo">
        <Highlight {...defaultProps} language="jsligo" code={JSLIGO_EXAMPLE} theme={prismTheme}>
          {({ className, style, tokens, getLineProps, getTokenProps }) => (
            <pre className={clsx(className, styles["code-examples__ide"])} style={style}>
              {tokens.map((line, i) => (
                <div {...getLineProps({ line, key: i })}>
                  {line.map((token, key) => (
                    <span {...getTokenProps({ token, key })} />
                  ))}
                </div>
              ))}
            </pre>
          )}
        </Highlight>
        <a
          href="https://ligo-webide-v2.gcp.marigold.dev/share/bc3fc573a724b6851dde8ec4607671f3"
          className={styles["code-examples__cta"]}
        >
          try jsligo online
        </a>
      </TabItem>

      <TabItem value="cameligo">
        <Highlight {...defaultProps} language="cameligo" code={CAMELIGO_EXAMPLE} theme={prismTheme}>
          {({ className, style, tokens, getLineProps, getTokenProps }) => (
            <pre className={clsx(className, styles["code-examples__ide"])} style={style}>
              {tokens.map((line, i) => (
                <div {...getLineProps({ line, key: i })}>
                  {line.map((token, key) => (
                    <span {...getTokenProps({ token, key })} />
                  ))}
                </div>
              ))}
            </pre>
          )}
        </Highlight>
        <a
          href="https://ligo-webide-v2.gcp.marigold.dev/share/4466658a67a99308725b4954ed51dc29"
          className={styles["code-examples__cta"]}
        >
          try cameligo online
        </a>
      </TabItem>
    </Tabs>
  );
}

export default function HomepageCodeExamples() {
  return (
    <div id="preview" className={styles["code-examples"]}>
      <CodeExamples />
    </div>
  );
}
