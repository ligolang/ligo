import React from "react";
import Highlight, { defaultProps } from "prism-react-renderer";
import Tabs from "@theme/Tabs";
import TabItem from "@theme/TabItem";
import { useColorMode } from "@docusaurus/theme-common";
import defaultTheme from "prism-react-renderer/themes/palenight";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import styles from './styles.module.css';

import CAMELIGO_EXAMPLE from '!!raw-loader!./cameligo.mligo';
import JSLIGO_EXAMPLE from '!!raw-loader!./jsligo.jsligo';

function CodeExamples(props) {
  const {
    siteConfig: {
      themeConfig: { prism = {} },
    },
  } = useDocusaurusContext();
  const lightModeTheme = prism.theme || defaultTheme;
  const darkModeTheme = prism.darkTheme || lightModeTheme;
  const prismTheme =
    useColorMode().colorMode === "dark" ? darkModeTheme : lightModeTheme;

  return (
    <Tabs
      defaultValue="jsligo"
      attributes={{className: styles.tabPannel}}
      values={[
        { label: "JsLIGO", value: "jsligo" },
        { label: "CameLIGO", value: "cameligo" },
        { label: "Try it", value: "try" }
      ]}
    >
      <TabItem value="jsligo">
        <Highlight
          {...defaultProps}
          language="jsligo"
          code={JSLIGO_EXAMPLE}
          theme={prismTheme}
        >
          {({ className, style, tokens, getLineProps, getTokenProps }) => (
            <pre className={className} style={style}>
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
      </TabItem>

      <TabItem value="cameligo">
        <Highlight
          {...defaultProps}
          language="cameligo"
          code={CAMELIGO_EXAMPLE}
          theme={prismTheme}
        >
          {({ className, style, tokens, getLineProps, getTokenProps }) => (
            <pre className={className} style={style}>
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
      </TabItem>
      <TabItem value="try">
        <div id="webide">
          <ul>
            <li className="primary">
              <a href="https://ligo-webide-v2.gcp.marigold.dev/share/168a39632c555e70098ac6236c7a20a8">JsLIGO on Web IDE</a>
            </li>
            <li className="secondary">
              <a href="https://ligo-webide-v2.gcp.marigold.dev/share/fd1ed87670420bfaa6438b2078f8de6c">CameLIGO on Web IDE</a>
            </li>
          </ul>
        </div>
      </TabItem>
    </Tabs >
  );
}

export default function HomepageCodeExamples() {
  return (
    <div id="preview">
      <CodeExamples />
    </div>
  );
}
