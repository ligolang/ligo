import React, { useState } from "react";
import SyntaxContext from "@theme/Syntax/SyntaxContext";

let defaultSyntax = (() => {
  if (typeof window === 'undefined') return "jsligo"

  const params = new Proxy(new URLSearchParams(window.location.search), {
    get: (searchParams, prop) => searchParams.get(prop),
  });

  const valid = ["jsligo", "cameligo", "reasonligo", "pascaligo"];

  const lang = (params.lang || "").toLowerCase();

  if (valid.includes(lang)) return lang;

  return "jsligo";
})();

// Default implementation, that you can customize
export default function Root({ children }) {
  const [syntax, setSyntax] = useState(defaultSyntax);

  return (
    <SyntaxContext.Provider value={{ syntax, setSyntax }}>
      {children}
    </SyntaxContext.Provider>
  );
}
