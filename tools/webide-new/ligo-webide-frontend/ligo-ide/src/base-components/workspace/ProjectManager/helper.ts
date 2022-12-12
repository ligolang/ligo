import { FileInfo, FolderInfo } from "~/base-components/file-ops";

const filterFolder = (file: (FolderInfo | FileInfo)[]) => {
  return file.filter((item) => item.type === "folder");
};

const filterFile = (file: (FolderInfo | FileInfo)[]) => {
  return file.filter((item) => item.type === "file");
};

const compareUnicode = (file: (FolderInfo | FileInfo)[]) => {
  return file.sort((a, b) => a.name.localeCompare(b.name));
};

const compose = (...funcs: ((_: (FolderInfo | FileInfo)[]) => (FolderInfo | FileInfo)[])[]) => {
  return funcs.reduce(
    (a, b) =>
      (...args) =>
        a(b(...args))
  );
};

const sortFile = (file: (FolderInfo | FileInfo)[]) => {
  const sortedFolder = compose(filterFolder, compareUnicode)(file);
  const sortedFile = compose(filterFile, compareUnicode)(file);
  return sortedFolder.concat(sortedFile);
};

const getProjectNumber = (baseProjectName: string, projectNameWithNumber: string) => {
  const isNumericRegex = new RegExp(`^${baseProjectName}\\((0|[1-9]{1}[0-9]*)\\)$`, "g");

  const isNumeric = projectNameWithNumber.match(isNumericRegex);
  if (!isNumeric) {
    return null;
  }
  let num = "";
  for (let i = projectNameWithNumber.length - 2; i >= 0; i--) {
    if (projectNameWithNumber[i] === "(") {
      break;
    }
    num += projectNameWithNumber[i];
  }
  return [...num].reverse().join("");
};

const getProjectName = (projectNameWithNumber: string) => {
  const isNumericRegex = /^.*\((0|[1-9]{1}[0-9]*)\)$/g;
  const isNumeric = projectNameWithNumber.match(isNumericRegex);
  if (!isNumeric) {
    return null;
  }
  let name = "";
  for (let i = 0; i < projectNameWithNumber.length - 1; i++) {
    if (projectNameWithNumber[i] === "(") {
      break;
    }
    name += projectNameWithNumber[i];
  }
  return name;
};

export { sortFile, getProjectName, getProjectNumber };
