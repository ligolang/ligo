import * as monaco from "monaco-editor";
import { registerRulesForLanguage } from "monaco-ace-tokenizer";
import { c_cppHighlightRules } from "./c_cpp";

import { Wat } from "./wat";

export default async function () {
  monaco.languages.register({
    id: "cpp-eosio",
  });
  registerRulesForLanguage("cpp-eosio", new c_cppHighlightRules());

  monaco.languages.onLanguage("wat", () => {
    monaco.languages.setMonarchTokensProvider("wat", Wat.MonarchDefinitions);
    monaco.languages.setLanguageConfiguration("wat", Wat.LanguageConfiguration);
    monaco.languages.registerCompletionItemProvider("wat", Wat.CompletionItemProvider);
    monaco.languages.registerHoverProvider("wat", Wat.HoverProvider);
  });
  monaco.languages.register({ id: "wat" });

  monaco.editor.defineTheme("eos-studio", {
    base: "vs-dark", // can also be vs-dark or hc-black
    inherit: true, // can also be false to completely replace the builtin rules
    colors: {
      "editor.foreground": "#F9FAF4",
      "editor.background": "#252527",
      "editor.selectionBackground": "#494950",
      "editor.lineHighlightBackground": "#2F2F32",
      "editorCursor.foreground": "#F9FAF4",
      "textLink.foreground": "#AD7AFF",
      focusBorder: "#AD7AFF",
      // "inputOption.activeBackground": "#AD7AFF88",
      // "input.border": "#AD7AFF",
      "menu.foreground": "#FFFFFF",
      "menu.background": "#252527",
      "menu.selectionBackground": "#8F55EC",
      "list.focusForeground": "#F9FAF4",
      "list.focusBackground": "#8F55EC",
      // "list.highlightForeground": "#F9FAF4",
    },
    rules: [
      { token: "", foreground: "F9FAF4", background: "252527" },
      { token: "keyword", foreground: "AD7AFF" },
      { token: "keyword.eosio", foreground: "AD7AFF", fontStyle: "bold" },
      { token: "keyword.operator", foreground: "F92672" },
      { token: "type", foreground: "66D9EF", fontStyle: "italic" },
      { token: "type.eosio", foreground: "66D9EF", fontStyle: "bold" },
      { token: "string", foreground: "CE9178" },
      { token: "constant", foreground: "E6DB74" },
      { token: "function", foreground: "AD7AFF" },
      // { token: 'comment.js', foreground: '008800', fontStyle: 'bold' },
      // { token: 'comment.css', foreground: '0000ff' } // will inherit fontStyle from `comment` above
    ],
  });

  monaco.languages.registerCodeActionProvider("cpp-eosio", {
    provideCodeActions: (model, range, context, token) => {
      const actions = context.markers
        .filter((error) => error.message.indexOf("did you mean") > -1)
        .map((error) => {
          const quickfix = error.message.split("did you mean")[1].split("'")[1];
          const codeAction = {
            title: `Replace with '${quickfix}'`,
            diagnostics: [error],
            kind: "quickfix",
            edit: {
              edits: [
                {
                  resource: model.uri,
                  // modelVersionId: model.getVersionId(),
                  edits: [{ range: error, text: quickfix }],
                },
              ],
            },
            isPreferred: true,
          };
          return codeAction;
        });

      const codeActionList = {
        actions,
        dispose: () => {},
      };
      return codeActionList;
    },
  });
}
