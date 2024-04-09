function _extends() { _extends = Object.assign ? Object.assign.bind() : function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; }; return _extends.apply(this, arguments); }

import Highlight, { defaultProps } from "prism-react-renderer";
import React, { useEffect, useState } from "react"; // THE PROBLEM IS USE THEME CONTEXT ==>>>>

import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import { SyntaxContext } from "@theme/Syntax";
import defaultTheme from "prism-react-renderer/themes/palenight";

const {
  Prism
} = require("prism-react-renderer");

Prism.languages = { ...Prism.languages,
  cameligo: { ...Prism.languages.ocaml,
    comment: [/(^|[^\\])\/\*[\s\S]*?\*\//, /\(\*[\s\S]*?\*\)/, /\/\/.*/]
  },
  jsligo: Prism.languages.typescript
};

function SyntaxTitle(props) {
  const {
    siteConfig: {
      themeConfig: {
        prism = {}
      }
    }
  } = useDocusaurusContext();
  const lightModeTheme = prism.singleTheme || defaultTheme; // todo fix Hook is called outside the <ColorModeProvider>. Please see https://docusaurus.io/docs/api/themes/configuration#use-color-mode.
  // const {colorMode, setColorMode} = useColorMode();
  // const prismTheme = colorMode === "dark" ? darkModeTheme : lightModeTheme;

  const [mounted, setMounted] = useState(false);
  useEffect(() => {
    setMounted(true);
  }, []);
  return /*#__PURE__*/React.createElement(SyntaxContext.Consumer, null, ({
    syntax
  }) => {
    if (syntax === props.syntax) {
      return /*#__PURE__*/React.createElement(Highlight, _extends({}, defaultProps, {
        key: mounted,
        language: props.syntax,
        code: props.children,
        theme: lightModeTheme
      }), ({
        className,
        tokens,
        getLineProps,
        getTokenProps
      }) => /*#__PURE__*/React.createElement("h4", {
        className: className,
        style: {
          backgroundColor: "var(--ifm-background-color)",
          fontFamily: "var(--ifm-font-family-monospace)",
          lineHeight: "var(--ifm-pre-line-height)",
          fontSize: "1.1rem",
          fontWeight: "bold",
          padding: 0,
          whiteSpace: "break-spaces",
          marginTop: "3rem"
        }
      }, tokens.map((line, i) => /*#__PURE__*/React.createElement("div", getLineProps({
        line,
        key: i
      }), line.map((token, key) => /*#__PURE__*/React.createElement("span", getTokenProps({
        token,
        key
      })))))));
    } else {
      return /*#__PURE__*/React.createElement("div", null);
    }
  });
}

export default SyntaxTitle;