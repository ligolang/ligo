const filterFolder = file => {
  return file.filter(item => item.type === "folder");
};

const filterFile = file => {
  return file.filter(item => item.type === "file");
};

const compareUnicode = file => {
  return file.sort((a, b) => a.name.localeCompare(b.name));
};

const compose = (...funcs) => {
  return funcs.reduce((a, b) => (...args) => a(b(...args)));
};

const sortFile = file => {
  const sortedFolder = compose(filterFolder, compareUnicode)(file);
  const sortedFile = compose(filterFile, compareUnicode)(file);
  return sortedFolder.concat(sortedFile);
};

export { sortFile };
