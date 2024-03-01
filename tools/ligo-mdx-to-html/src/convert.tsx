#!/usr/bin/env node
import * as fs from "fs";
import * as path from "path";
import { SafeMdxRenderer } from "safe-mdx";
import { renderToString } from "react-dom/server";
import * as components from "./components.js";
import { embeddedCss } from "./css.js";
import { mk_nav, navArgs, navFileTree } from "./nav.js";
import { args } from "./cli.js";
import { compare_file_names, getExt, setExtHtml } from "./util.js";

export function Page(
  mdxContent: string,
  targetSyntax: components.syntax,
  navArgs?: navArgs
) {
  const nav = navArgs ? mk_nav(navArgs) : null;
  return (
    <html>
      <head>
        <style>{embeddedCss}</style>
      </head>
      <body>
        {nav}
        <article className="markdown-body">
          <SafeMdxRenderer
            code={mdxContent}
            components={{
              Syntax(props: any) {
                return components.Syntax(props, targetSyntax);
              },
              Spoiler(props: any) {
                return components.Spoiler(props, targetSyntax);
              },
              SyntaxTitle(props: any) {
                return components.SyntaxTitle(props, targetSyntax);
              },
              Box(props: any) {
                return components.Box(props);
              },
              a(props: any) {
                if (
                  getExt(props.href) == ".md" ||
                  getExt(props.href) == ".mdx"
                ) {
                  return <a {...{ ...props, href: setExtHtml(props.href) }} />;
                }
                return <a {...props} />;
              },
            }}
          />
        </article>
      </body>
    </html>
  );
}

if (args.source_file != null) {
  // single-file mode
  const filepath = args.source_file;

  const mdxContent = fs.readFileSync(filepath, "utf8");

  if (args.output_path == null) {
    console.log(renderToString(Page(mdxContent, args.syntax)));
  } else {
    fs.writeFileSync(
      args.output_path,
      renderToString(Page(mdxContent, args.syntax))
    );
  }
} else if (args.source_dir != null) {
  const source_dir = args.source_dir;
  const output_dir = args.output_path || source_dir;

  // collects (relative) paths to markdown files in a directory in 2 formats
  function collectFilesAndNavTree(
    rel_dir: string // relative to source_dir
  ): [Array<string>, navFileTree] {
    var collected: Array<string> = [];
    var collected_tree: navFileTree = [];
    fs.readdirSync(path.join(source_dir, rel_dir))
      .sort((a, b) => compare_file_names(a, b))
      .forEach((name) => {
        const p = path.join(rel_dir, name);
        if (fs.lstatSync(path.join(source_dir, p)).isDirectory()) {
          const [files, tree] = collectFilesAndNavTree(p);
          collected = collected.concat(files);
          collected_tree.push([name, tree]);
        } else if (path.extname(p) == ".md" || path.extname(p) == ".mdx") {
          collected.push(p);
          collected_tree.push([p, null]);
        }
      });
    return [collected, collected_tree];
  }
  let [files, nav_files] = collectFilesAndNavTree("");
  files.forEach((rel_path) => {
    const filepath = path.join(source_dir, rel_path);
    const output_path = setExtHtml(path.join(output_dir, rel_path));
    if (args.verbose) {
      console.log(filepath + " -> " + output_path);
    }
    const mdxContent = fs.readFileSync(filepath, "utf8");
    const navArgs = {
      files: nav_files,
      active: rel_path,
      nestedness: rel_path.split("/").length - 1,
    };
    fs.mkdirSync(path.dirname(output_path), { recursive: true });
    fs.writeFileSync(
      output_path,
      renderToString(Page(mdxContent, args.syntax, navArgs))
    );
  });
} else throw new Error("No target, please specify source_dir or source_file");
