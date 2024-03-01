import { basename } from "path";
import { removeExt, setExtHtml, unnest } from "./util.js";

let key_counter = 0;

function get_key() {
  key_counter += 1;
  return key_counter;
}

export type navFileTree = Array<[string, navFileTree | null]>;

export type navArgs = {
  files: navFileTree;
  active?: string;
  nestedness?: number;
};

export function folder([name, contents]: [string, navFileTree], args: navArgs) {
  return [
    <li className="heading" key={get_key()}>
      <span>{name}</span>
    </li>,
    <li key={get_key()}>
      <ul> {contents.map((x) => process(x, args))}</ul>
    </li>,
  ];
}

export function file(name: string, args: navArgs) {
  return (
    <li key={get_key()}>
      <a className="" href={unnest(setExtHtml(name), args.nestedness || 0)}>
        {removeExt(basename(name))}
      </a>
    </li>
  );
}

export function process(
  [name, contents]: [string, navFileTree | null],
  args: navArgs
) {
  switch (contents) {
    case null:
      return file(name, args);
    default:
      return folder([name, contents], args);
  }
}

export function mk_nav(args: navArgs) {
  return (
    <nav id="menu">
      <ul>{args.files.map((file) => process(file, args))}</ul>
    </nav>
  );
}
