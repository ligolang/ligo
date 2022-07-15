import * as monaco from "monaco-editor";
import { registerRulesForLanguage } from "monaco-ace-tokenizer";

import prettier from "prettier/standalone";
import solidityPlugin from "prettier-plugin-solidity";

import SolidityHighlightRules from "./SolidityHighlightRules";

export default function () {
  monaco.languages.typescript.javascriptDefaults.setDiagnosticsOptions({
    noSemanticValidation: true,
    noSyntaxValidation: true,
  }); // TODO use it only for ligo code
  monaco.languages.register({ id: "solidity" });
  monaco.languages.setLanguageConfiguration("solidity", {
    comments: {
      lineComment: "//",
      blockComment: ["/*", "*/"],
    },
    brackets: [
      ["(", ")"],
      ["[", "]"],
      ["{", "}"],
    ],
    autoClosingPairs: [
      { open: "(", close: ")" },
      { open: "[", close: "]" },
      { open: "{", close: "}" },
    ],
  });
  registerRulesForLanguage("solidity", new SolidityHighlightRules());

  monaco.languages.registerDocumentFormattingEditProvider("solidity", {
    async provideDocumentFormattingEdits(model) {
      const code = model.getValue();

      const formatted = prettier.format(code, {
        parser: "solidity-parse",
        plugins: [solidityPlugin],
        tabWidth: 2,
      });

      return [
        {
          range: model.getFullModelRange(),
          text: formatted,
        },
      ];
    },
  });
}
