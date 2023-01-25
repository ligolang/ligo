import React, { useEffect, useState } from 'react';
import Highlight, { defaultProps } from "prism-react-renderer";

// THE PROBLEM IS USE THEME CONTEXT ==>>>>
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import { useColorMode } from '@docusaurus/theme-common';
import { SyntaxContext } from '@theme/Syntax';

import defaultTheme from 'prism-react-renderer/themes/palenight';

const { Prism } = require("prism-react-renderer");

Prism.languages = {
  ...Prism.languages,
  cameligo: {
    ...Prism.languages.ocaml,
    'comment': [
      /(^|[^\\])\/\*[\s\S]*?\*\//,
      /\(\*[\s\S]*?\*\)/,
      /\/\/.*/
    ]
  },
  jsligo: Prism.languages.typescript
};


function SyntaxTitle(props) {
  const {
    siteConfig: {
      themeConfig: { prism = {} },
    }
  } = useDocusaurusContext();

  const lightModeTheme = prism.singleTheme || defaultTheme;
  // todo fix Hook is called outside the <ColorModeProvider>. Please see https://docusaurus.io/docs/api/themes/configuration#use-color-mode.
  // const {colorMode, setColorMode} = useColorMode();
  // const prismTheme = colorMode === "dark" ? darkModeTheme : lightModeTheme;

  const [mounted, setMounted] = useState(false);

  useEffect(() => {
    setMounted(true);
  }, []);

  return (
    <SyntaxContext.Consumer>
      {(({ syntax }) => {
        if (syntax === props.syntax) {
          return (
            <Highlight {...defaultProps} key={mounted} language={props.syntax} code={props.children} theme={lightModeTheme}>
              {({ className, tokens, getLineProps, getTokenProps }) => (
                <pre className={className} style={{ backgroundColor: 'var(--ifm-background-color)', fontSize: '1.1rem', fontWeight: 'bold', padding: 0, whiteSpace: 'break-spaces', marginTop: '3rem' }}>
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
          )
        } else {
          return <div></div>
        }
      })}
    </SyntaxContext.Consumer>
  );
}

export default SyntaxTitle;
