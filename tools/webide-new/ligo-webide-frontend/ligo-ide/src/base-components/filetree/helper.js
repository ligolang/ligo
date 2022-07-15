const computeType = valueArr => {
  const errorCount = valueArr.filter(item => item.type === "error").length;
  const warningCount = valueArr.filter(item => item.type === "warning").length;

  return {
    type: errorCount ? "error" : warningCount ? "warning" : "default",
    count: errorCount || warningCount || -1,
  };
};

const updateEachNode = (typeInfo, pathInfo, treeMap) => {
  const refreshNode = (cur, nodeValue, isLeaf) => {
    if (!nodeValue) {
      treeMap[cur] = {
        name: cur,
        type: typeInfo.type,
        isLeaf,
      };
      if (isLeaf) {
        treeMap[cur].count = typeInfo.count;
      }
    } else {
      const oldDefault = nodeValue.type === "default";
      const oldError = nodeValue.type === "error";
      const oldWarning = !oldDefault && !oldError;
      const newError = typeInfo.type === "error";
      if (oldError) {
        return;
      }
      if (oldWarning) {
        nodeValue.type = newError ? "error" : "warning";
        if (isLeaf) {
          nodeValue.count = newError ? typeInfo.count : nodeValue.count;
        }
        return;
      }
      if (oldDefault) {
        nodeValue.type = typeInfo.type;
        if (isLeaf) {
          nodeValue.count = typeInfo.count;
        }
      }
    }
  };

  pathInfo.forEach((cur, index) => {
    const isLeaf = index === pathInfo.length - 1;
    const nodeValue = treeMap[cur];
    refreshNode(cur, nodeValue, isLeaf);
  });
};

const updateErrorInfo = (decorations, fileKey) => {
  const keyArr = Object.keys(decorations);

  return keyArr.reduce((prev, cur) => {
    const typeInfo = computeType(decorations[cur]);
    const pathInfo = cur.replace(`${fileKey}/`, "").split("/");

    updateEachNode(typeInfo, pathInfo, prev);
    return prev;
  }, {});
};

const travelTree = (treeData, fn, extraValue) => {
  // console.log('travelTree', treeData, extraValue)
  const travel = (tree, fn) => {
    const shouldStop = fn(tree, extraValue);
    if (!tree.children || shouldStop) return;
    for (let i = 0; i < tree.children.length; i++) {
      travel(tree.children[i], fn);
    }
  };
  travel(treeData, fn);
};

const findInTree = (treeData, fn) => {
  const find = (tree, fn) => {
    for (const node of tree) {
      if (fn(node)) {
        return node;
      }
      if (node.children) {
        const result = find(node.children, fn);
        if (result) {
          return result;
        }
      }
    }
  };
  return find(treeData, fn);
};

const mapTree = (treeData, fn) => {
  const mapT = (tree, fn) => {
    for (const node of tree) {
      fn(node);
      if (node.children) {
        mapT(node.children, fn);
      }
    }
  };
  return mapT(treeData, fn);
};

const checkFatherNode = (curNode, targetNode, fn) => {
  if (!curNode.children) return true;
  if (curNode.path === targetNode.fatherPath) {
    fn(curNode, targetNode);
    return true;
  }
  if (curNode.path === targetNode.path && curNode.root && targetNode.root) {
    fn(curNode, targetNode);
    return true;
  }
  return false;
};

const findFather = fn => (curNode, targetNode) => checkFatherNode(curNode, targetNode, fn);

const findChildren = (treeData, path) => treeData.children.find(item => item.path === path);

const filterDuplicate = arr => Array.from(new Set(arr));

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

export {
  updateErrorInfo,
  travelTree,
  findFather,
  findChildren,
  filterDuplicate,
  findInTree,
  sortFile,
  mapTree,
};
