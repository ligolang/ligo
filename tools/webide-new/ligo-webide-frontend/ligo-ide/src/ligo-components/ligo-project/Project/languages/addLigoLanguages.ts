import * as monaco from "monaco-editor";
import { Registry } from "monaco-textmate";
import { wireTmGrammars } from "monaco-editor-textmate";

import tzTm from "./syntaxes/michelson.tmLanguage.json";
import mligoTm from "./syntaxes/mligo.tmLanguage.json";
import mligoConfiguration from "./syntaxes/mligo.configuration.json";
import jsligoTm from "./syntaxes/jsligo.tmLanguage.json";
import jsligoConfiguration from "./syntaxes/jsligo.configuration.json";
import ligoTm from "./syntaxes/ligo.tmLanguage.json";
import ligoConfiguration from "./syntaxes/ligo.configuration.json";
import { LangConfiguration } from "./type";

const convertConfiguration = (conf: {
  comments: { lineComment: string; blockComment: string[] };
  brackets: string[][];
  autoClosingPairs: string[][];
  surroundingPairs: string[][];
}): LangConfiguration => {
  return {
    comments: {
      lineComment: conf.comments.lineComment,
      blockComment: [conf.comments.blockComment[0], conf.comments.blockComment[1]],
    },
    brackets: conf.brackets.map((p) => [p[0], p[1]]),
    autoClosingPairs: conf.autoClosingPairs.map((p) => {
      return {
        open: p[0],
        close: p[1],
      };
    }),
    surroundingPairs: conf.surroundingPairs.map((p) => {
      return {
        open: p[0],
        close: p[1],
      };
    }),
  };
};

export const addLigoLanguages = async (editor: monaco.editor.ICodeEditor) => {
  monaco.languages.register({ id: "pascaligoext" });
  monaco.languages.register({ id: "cameligoext" });
  monaco.languages.register({ id: "jsligoext" });
  monaco.languages.register({ id: "tzext" });

  monaco.languages.setLanguageConfiguration(
    "pascaligoext",
    convertConfiguration(ligoConfiguration)
  );
  monaco.languages.setLanguageConfiguration(
    "cameligoext",
    convertConfiguration(mligoConfiguration)
  );
  monaco.languages.setLanguageConfiguration("jsligoext", convertConfiguration(jsligoConfiguration));
  monaco.languages.setLanguageConfiguration("tzext", convertConfiguration(jsligoConfiguration));

  const ligoRegistry = new Registry({
    getGrammarDefinition: async () => {
      return new Promise((resolve) => {
        resolve({
          format: "json",
          content: ligoTm,
        });
      });
    },
  });

  const mligoRegistry = new Registry({
    getGrammarDefinition: async () => {
      return new Promise((resolve) => {
        resolve({
          format: "json",
          content: mligoTm,
        });
      });
    },
  });

  const jsligoRegistry = new Registry({
    getGrammarDefinition: async () => {
      return new Promise((resolve) => {
        resolve({
          format: "json",
          content: jsligoTm,
        });
      });
    },
  });

  const tzRegistry = new Registry({
    getGrammarDefinition: async () => {
      return new Promise((resolve) => {
        resolve({
          format: "json",
          content: tzTm,
        });
      });
    },
  });

  const ligoGrammars = new Map<string, string>();
  ligoGrammars.set("pascaligoext", "source.ligo");

  const mligoGrammars = new Map<string, string>();
  mligoGrammars.set("cameligoext", "source.mligo");

  const jsligoGrammars = new Map<string, string>();
  jsligoGrammars.set("jsligoext", "source.jsligo");

  const tzGrammars = new Map<string, string>();
  tzGrammars.set("tzext", "source.michelson");

  await wireTmGrammars(monaco, ligoRegistry, ligoGrammars, editor);
  await wireTmGrammars(monaco, mligoRegistry, mligoGrammars, editor);
  await wireTmGrammars(monaco, jsligoRegistry, jsligoGrammars, editor);
  await wireTmGrammars(monaco, tzRegistry, tzGrammars, editor);
};
