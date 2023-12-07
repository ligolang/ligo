import React, { useContext } from "react";
const valid = ["jsligo", "cameligo", "pascaligo"];
const ctx = {
  syntax: (() => {
    if (typeof window === "undefined") return "jsligo";
    const syntax = localStorage.getItem("syntax");
    const params = new Proxy(new URLSearchParams(window.location.search), {
      get: (searchParams, prop) => searchParams.get(prop)
    });
    const lang = (params.lang || "").toLowerCase();
    if (valid.includes(lang)) return lang;
    return syntax ?? "jsligo";
  })(),
  setSyntax: () => {}
};
const SyntaxContext = /*#__PURE__*/React.createContext(ctx);
export const useSyntax = () => {
  const syntaxContext = useContext(SyntaxContext);
  return syntaxContext;
};
export default SyntaxContext;