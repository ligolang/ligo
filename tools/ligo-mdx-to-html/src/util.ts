import * as path from "path";

export function getExt(fileName: string) {
  var pos = fileName.lastIndexOf(".");
  return fileName.slice(pos);
}
export function removeExt(fileName: string) {
  var pos = fileName.lastIndexOf(".");
  return fileName.slice(0, pos);
}

// setExtHtml("a.mligo.md") == "a.mligo.html"
export function setExtHtml(fileName: string) {
  return removeExt(fileName) + ".html";
}

export function unnest(file: string, n: number) {
  return path.join(...Array(n).fill(".."), file);
}

// e.g. X.Y.html should come after X.html which is not true in lexicographical order
// also file with top-level declarations should be first
export function compare_file_names(a: string, b: string) {
  const a_noext = removeExt(a);
  const b_noext = removeExt(b);
  if (a_noext == "toplevel") {
    return -1;
  }
  if (b_noext == "toplevel") {
    return 1;
  }
  return a_noext.localeCompare(b_noext);
}
