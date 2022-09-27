import { FileInfo, FolderInfo } from "~base-components/file-ops";

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

export { sortFile };
