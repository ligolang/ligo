import * as components from "./components.js";
import { parse } from "ts-command-line-args";

export interface Args {
  source_file?: string;
  source_dir?: string;
  output_path?: string;
  syntax: components.syntax;
  help?: boolean;
  verbose?: boolean;
}

export function parseSyntax(str: string): components.syntax {
  switch (str) {
    case "jsligo":
      return "jsligo";
    case "cameligo":
      return "cameligo";
    default:
      throw new Error("Unknown syntax: " + str);
  }
}

export const args = parse<Args>(
  {
    source_file: { type: String, optional: true, alias: "f" },
    source_dir: { type: String, optional: true, alias: "d" },
    output_path: { type: String, optional: true, alias: "o" },
    syntax: { type: parseSyntax },
    help: {
      type: Boolean,
      optional: true,
      alias: "h",
      description: "Prints this usage guide",
    },
    verbose: {
      type: Boolean,
      optional: true,
      alias: "v",
      description: "Print debug info",
    },
  },
  {
    helpArg: "help",
    headerContentSections: [
      {
        header: "ligo doc mdx -> html converter",
        content:
          "use `ligo-mdx-to-xtml -d <dir>` to run on a directory or `ligo-mdx-to-xtml -f <file>`",
      },
    ],
  }
);
