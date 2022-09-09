import React, { useEffect, useState, useRef, forwardRef, useImperativeHandle } from "react";
import Tree from "rc-tree";
import cloneDeep from "lodash/cloneDeep";
import "./styles.css";
import { Menu, Item, useContextMenu, Separator } from "react-contexify";
import "react-contexify/dist/ReactContexify.min.css";
import PropTypes from "prop-types";
import { useHotkeys } from "react-hotkeys-hook";
import StatusTitle from "./statusTitle";
import { travelTree, updateErrorInfo, findChildren, findInTree, mapTree, sortFile } from "./helper";
import { modelSessionManager } from "~/base-components/code-editor";

let disableSetActive = false; // stop useless setActive function when filetree trigger onSelect event

const renderIcon = ({ data }) => {
  if (data.isLeaf) {
    return <i className="fas fa-file-code fa-fw mr-1" />;
  }
};

const renderSwitcherIcon = ({ loading, expanded, data }) => {
  if (loading && !data.isLeaf) {
    return (
      <span key="loading">
        <span className="fas fa-sm fa-spin fa-spinner fa-fw" />
      </span>
    );
  }

  if (data.isLeaf) {
    return null;
  }

  return expanded ? (
    <span key="switch-expanded">
      <span className="fas fa-chevron-down fa-fw" />
    </span>
  ) : (
    <span key="switch-close">
      <span className="fas fa-chevron-right fa-fw" />
    </span>
  );
};

const setLeaf = (treeData, curKey) => {
  const loopLeaf = (data) => {
    data.forEach((item) => {
      if (!item.key) {
        item.key = item.path;
      }
      if (!item.title) {
        item.title = item.name;
      }
      if (
        item.path.length > curKey.length
          ? item.path.indexOf(curKey) !== 0
          : curKey.indexOf(item.path) !== 0
      ) {
        return;
      }
      if (item.children) {
        loopLeaf(item.children);
      } else if (!item.children || item.children.length === 0) {
        item.isLeaf = true;
      }
    });
  };
  loopLeaf(treeData);
};

const FileTree = forwardRef(({ projectManager, onSelect, initialPath, contextMenu }, ref) => {
  const treeRef = React.useRef();
  const [treeData, setTreeData] = useState([]);
  const treeDataRef = useRef();
  treeDataRef.current = treeData;
  const [autoExpandParent, setAutoExpandParent] = useState(true);
  const [expandedKeys, setExpandKeys] = useState([]);
  const expandKeysRef = useRef();
  expandKeysRef.current = expandedKeys;
  const [selectedKeys, setSelectedKeys] = useState([initialPath]);
  const [persistDOM, setPersist] = useState(null);
  const [selectNode, setSelectNode] = useState(null);
  const [rightClickNode, setRightClikNode] = useState(null);
  const [copyNode, setCopyNode] = useState(null);
  const [moveNode, setMoveNode] = useState(null);
  const [isCopy, setIsCopy] = useState(true);
  const [dragTarget, setDragTarget] = useState("");
  const [prevDragEnter, setPrevDragEnter] = useState("");
  const [isBlankAreaRightClick, setIsBlankAreaRightClick] = useState(false);
  const [isTreeDataRoot, setIsTreeDataRoot] = useState(false);
  const [targetForExpand, setTargetForExpand] = useState(null);
  const targetForExpandRef = useRef();
  targetForExpandRef.current = targetForExpand;

  let treeNodeContextMenu =
    typeof contextMenu === "function" ? contextMenu(rightClickNode) : contextMenu;

  isBlankAreaRightClick &&
    (treeNodeContextMenu = treeNodeContextMenu.filter(
      (item) => item && (item.text === "New File" || item.text === "New Folder")
    ));
  // Removing rename and delete operations from the root of the file tree
  if (!isBlankAreaRightClick && isTreeDataRoot) {
    const renameAndDeleteText = ["Rename", "Delete"];
    treeNodeContextMenu = treeNodeContextMenu.filter((item) => {
      return item ? !renameAndDeleteText.includes(item.text) : treeNodeContextMenu.push(null);
    });
    !treeNodeContextMenu.slice(-1)[0] && treeNodeContextMenu.pop();
  }

  const { show, hideAll } = useContextMenu({
    id: "file-tree",
  });

  useImperativeHandle(ref, () => ({
    setActive(key) {
      if (!key) {
        setSelectedKeys([]);
        setSelectNode(null);
        return;
      }
      handleSetActive(key);
    },
    get activeNode() {
      return selectNode;
    },
    get rootNode() {
      return treeData;
    },
    updateTreeTitle() {
      updateTitle(...treeData);
    },
  }));

  useEffect(async () => {
    await initTree();
  }, []);

  const handleContextMenu = ({ event, node }) => {
    node.root ? setIsTreeDataRoot(true) : setIsTreeDataRoot(false);
    addPersist(event);
    event.nativeEvent.preventDefault();
    event.stopPropagation();
    setIsBlankAreaRightClick(false);
    setRightClikNode(node);
    show(event.nativeEvent, {
      props: {
        key: "value",
      },
    });
  };

  const handleEmptyTreeContextMenu = (event) => {
    if (event.target.className.includes("react-contexify")) return;
    setIsBlankAreaRightClick(true);
    setRightClikNode(treeData[0]);
    removePersist();
    show(event.nativeEvent, {
      props: {
        key: "value",
      },
    });
  };

  const handleMenuItemClick = (item) => {
    return ({ event }) => {
      item.onClick(rightClickNode);
      event.stopPropagation();
      hideAll();
    };
  };

  const renderMenu = (treeNodeContextMenu) => {
    return treeNodeContextMenu.map((item, index) =>
      item ? (
        <Item key={item.text} onClick={handleMenuItemClick(item)}>
          {item.text}
        </Item>
      ) : (
        <Separator key={`blank-${index}`} />
      )
    );
  };

  const removePersist = () => {
    if (persistDOM) {
      persistDOM.className = persistDOM.className.toString().replace(" persist--active", "");
      setPersist(null);
    }
  };

  const addPersist = (event) => {
    removePersist();
    event.currentTarget.parentElement.className += " persist--active";
    setPersist(event.currentTarget.parentElement);
  };

  const rootClick = () => {
    removePersist();
  };

  const renderTitle = (curNode, errorNode) => {
    const matchedValue = errorNode[curNode.name];
    if (!matchedValue) return;
    matchedValue.type === "default"
      ? (curNode.title = curNode.name)
      : (curNode.title = (
          <StatusTitle
            title={curNode.name}
            isLeaf={matchedValue.isLeaf}
            showType={matchedValue.type}
            count={matchedValue.count}
          />
        ));
  };

  const updateTitle = (treeData) => {
    const rawDecoration = modelSessionManager.decorationMap;
    const hasError = Object.keys(rawDecoration).length !== 0;
    if (!hasError) return;
    const errorNode = updateErrorInfo(rawDecoration, treeData.key);
    travelTree(treeData, renderTitle, errorNode);
    setTreeData([treeData]);
  };

  const refreshDirectory = async (data) => {
    if (data.type === "newFile" || data.type === "newDirectory") {
      const tempTree = cloneDeep(treeDataRef.current);
      const newNode =
        data.type === "newFile"
          ? {
              type: "file",
              title: data.name,
              key: data.path,
              name: data.name,
              path: data.path,
              remote: true,
              isLeaf: true,
            }
          : {
              type: "folder",
              title: data.name,
              key: data.path,
              children: [],
              isLeaf: false,
              name: data.name,
              path: data.path,
              loading: true,
              remote: true,
            };
      const parentNode = findInTree(tempTree, (node) => node.path === data.basePath);

      if (parentNode) {
        parentNode.children.push(newNode);
        parentNode.children = sortFile(parentNode.children);
        setTreeData([...tempTree]);
      }
    }

    if (data.type === "renameFile" || data.type === "renameDirectory") {
      const tempTree = cloneDeep(treeDataRef.current);
      const node = findInTree(tempTree, (node) => node.path === data.oldPath);
      if (node) {
        node.title = data.newName;
        node.key = data.newPath;
        node.name = data.newName;
        node.path = data.newPath;
      }
      if (node && data.type === "renameDirectory") {
        mapTree(node.children, (nd) => {
          nd.key = nd.key.replace(data.oldPath, data.newPath);
          nd.path = nd.path.replace(data.oldPath, data.newPath);
        });
      }
      if (node) {
        setTreeData([...tempTree]);
        if (data.type === "renameDirectory") {
          setExpandKeys(
            expandKeysRef.current.map((key) => key.replace(data.oldPath, data.newPath))
          );
        }
      }
    }

    if (data.type === "deleteFile" || data.type === "deleteDirectory") {
      const tempTree = cloneDeep(treeDataRef.current);
      const parentNode = findInTree(
        tempTree,
        (node) => node.path === data.path.substring(0, data.path.lastIndexOf("/"))
      );
      if (parentNode) {
        parentNode.children = parentNode.children.filter((node) => node.path !== data.path);
        setTreeData([...tempTree]);

        if (data.type === "deleteDirectory") {
          setExpandKeys(expandKeysRef.current.filter((key) => !key.includes(data.path)));
        }
      }
    }

    if (
      data.type === "moveFile" ||
      data.type === "moveDirectory" ||
      data.type === "copyFile" ||
      data.type === "copyDirectory"
    ) {
      const tempTree = cloneDeep(treeDataRef.current);
      const targetNode = cloneDeep(findInTree(tempTree, (node) => node.path === data.targetPath));
      let newExpandKeys = expandKeysRef.current;
      const parentTargetPath = data.targetPath.substring(0, data.targetPath.lastIndexOf("/"));
      const parentDropPath = data.dropPath.substring(0, data.dropPath.lastIndexOf("/"));

      if (data.type === "moveFile" || data.type === "moveDirectory") {
        const parentNode = findInTree(tempTree, (node) => node.path === parentTargetPath);
        if (parentNode) {
          parentNode.children = parentNode.children.filter((node) => node.path !== data.targetPath);

          if (data.type === "moveDirectory") {
            newExpandKeys = newExpandKeys.filter((key) => !key.includes(data.targetPath));
          }
        }
      }

      if (targetNode) {
        mapTree([targetNode], (nd) => {
          nd.key = nd.key.replace(data.targetPath, data.dropPath);
          nd.path = nd.path.replace(data.targetPath, data.dropPath);
        });

        newExpandKeys = newExpandKeys.map((key) => key.replace(data.targetPath, data.dropPath));
      }

      const dropNode = findInTree(tempTree, (node) => node.path === parentDropPath);

      if (dropNode) {
        dropNode.children.push(targetNode);
        dropNode.children = sortFile(dropNode.children);
      }

      setTreeData([...tempTree]);
      setExpandKeys(newExpandKeys);
    }
  };

  const initTree = async () => {
    projectManager.onRefreshDirectory(refreshDirectory);
    await fetchTreeData(true);
  };

  const fetchTreeData = async (initFetch = false) => {
    const treeData = await projectManager.loadProjectFileTree();
    setLeaf([treeData], treeData.path);
    setTreeData([treeData]);
    setExpandKeys([treeData.path]);

    treeRef.current.setUncontrolledState({ indent: 0 });
    if (initFetch) {
      setSelectedKeys([initialPath]);
      setSelectNode(findChildren(treeData, initialPath));
    }
  };

  const handleClick = (event, node) => {
    disableSetActive = node.isLeaf;
    clickFolderNode(event, node);
    setIsBlankAreaRightClick(false);
    setRightClikNode(node);
  };

  const clickFolderNode = (event, node) => {
    setSelectNode(node);
    setSelectedKeys([node.path]);
    if (node.isLeaf) return;
    if (treeRef.current) {
      treeRef.current.onNodeExpand(event, node);
    }
  };

  const handleExpand = (keys, { node }) => {
    if (node.root || !!dragTarget || node.isLeaf) return;
    setAutoExpandParent(false);
    setExpandKeys(keys);
    setSelectNode(node);
  };

  const handleSetActive = (activeKey) => {
    const rootPath = projectManager.projectRoot;
    const hasRootPath = activeKey.includes(rootPath);
    const fileteredKey = hasRootPath
      ? activeKey.replace(`${projectManager.projectRoot}/`, "")
      : activeKey;
    const folderArr = fileteredKey.split("/").reduce((prev, cur, index) => {
      if (index === 0) {
        hasRootPath ? prev.push(`${rootPath}/${cur}`) : prev.push(cur);
        return prev;
      }
      prev.push(`${prev[index - 1]}/${cur}`);
      return prev;
    }, []);
    if (disableSetActive) {
      disableSetActive = false;
      return;
    }

    const findNodeByKey = (curNode, nodeKey) => {
      let stop = false;
      if (curNode.path === nodeKey) {
        setSelectNode(curNode);
        folderArr.forEach((item) => {
          if (!expandedKeys.includes(item) && item !== nodeKey) {
            setExpandKeys([...expandedKeys, item]);
          }
        });
        stop = true;
      }
      return stop;
    };
    !selectedKeys.includes(activeKey) && setSelectedKeys([activeKey]);
    !selectNode.path !== activeKey && travelTree(...treeData, findNodeByKey, activeKey);
  };

  const handleSelect = (_, { node }) => {
    setSelectNode(node);
    node.isLeaf && onSelect(node);
  };

  const enableHighLightBlock = (tree, needHighLight) => {
    let firstNode;
    let lastNode;
    const refreshClassName = (node) => {
      if (!firstNode) {
        firstNode = node;
      }
      lastNode = node;

      if (node.className) {
        node.className = needHighLight
          ? `${node.className} node--highlight`
          : node.className
              .replaceAll("node--highlight-first", "")
              .replaceAll("node--highlight-last", "")
              .replaceAll("node--highlight", "");
      } else {
        node.className = needHighLight ? "node--highlight" : "";
      }
    };

    mapTree([tree], refreshClassName);

    if (firstNode && lastNode && needHighLight) {
      firstNode.className = firstNode.className
        ? `${firstNode.className} node--highlight-first`
        : "node--highlight-first";
      lastNode.className = lastNode.className
        ? `${lastNode.className} node--highlight-last`
        : "node--highlight-last";
    }
  };

  const disableChildren = (node, needDisable) => {
    node.className = needDisable
      ? `${node.className} node--disable`
      : node.className.replaceAll("node--disable", "");
    if (node.children) {
      node.children.forEach((item) => {
        disableChildren(item, needDisable);
      });
    }
  };

  const handleDragStart = ({ event, node }) => {
    setDragTarget(node);
    disableChildren(node, true);
    event.dataTransfer.effectAllowed = "copyMove";
    event.currentTarget.id = "drag--active";
    event.dataTransfer.setDragImage(event.target, 3, 5);
    setSelectedKeys([]);
  };

  const handleDragEnter = ({ node }) => {
    if (prevDragEnter && node.path === prevDragEnter.path) {
      return;
    }

    if (node.path.includes(dragTarget.path)) {
      if (prevDragEnter) {
        const prevN = findInTree(treeData, (treeNode) => treeNode.path === prevDragEnter.path);
        prevN && enableHighLightBlock(prevN, false);
      }
      setPrevDragEnter(undefined);
      return;
    }

    setTargetForExpand(null);

    const fatherOrSelf = findInTree(
      treeData,
      (treeNode) =>
        treeNode.path === (node.type === "folder" || node.root ? node.path : node.fatherPath)
    );
    if (fatherOrSelf) {
      if (prevDragEnter && fatherOrSelf.path === prevDragEnter.path) {
        return;
      }

      if (prevDragEnter) {
        const prevN = findInTree(treeData, (treeNode) => treeNode.path === prevDragEnter.path);
        prevN && enableHighLightBlock(prevN, false);
      }
      const isExist = expandedKeys.includes(fatherOrSelf.path);
      if (!isExist) {
        setPrevDragEnter(fatherOrSelf);

        setTargetForExpand(fatherOrSelf);
        setTimeout(() => {
          if (targetForExpandRef.current && targetForExpandRef.current.path === fatherOrSelf.path) {
            setExpandKeys([...expandedKeys, fatherOrSelf.path]);
            enableHighLightBlock(fatherOrSelf, true);
          }
          setTargetForExpand(null);
        }, 500);
      } else {
        enableHighLightBlock(fatherOrSelf, true);
        setPrevDragEnter(fatherOrSelf);
      }
    }
  };

  const handleDragOver = ({ event, node }) => {
    setIsCopy(event.altKey);
    event.dataTransfer.dropEffect = event.altKey ? "copy" : "move";
  };

  const handleDragEnd = ({ event, node }) => {
    enableHighLightBlock(...treeData, false);
    const prevN = findInTree(treeData, (treeNode) => treeNode.path === dragTarget.path);
    disableChildren(prevN, false);
    setDragTarget("");
    event.currentTarget.id = "";
    setIsCopy(false);
  };

  const handleDrop = ({ node, dragNode }) => {
    let targetFolderPath = node.type === "folder" || node.root ? node.path : node.fatherPath;

    if (
      dragNode.path.substring(0, dragNode.path.lastIndexOf("/")) === targetFolderPath ||
      node.path.includes(dragNode.path)
    ) {
      return;
    }

    isCopy
      ? projectManager.copyOps(dragNode.path, `${targetFolderPath}/${dragNode.name}`, dragNode.type)
      : projectManager.moveOps(
          dragNode.path,
          `${targetFolderPath}/${dragNode.name}`,
          dragNode.type
        );
  };

  const handleMouseEnter = ({ event }) => {
    if (!dragTarget) {
      event.currentTarget.parentElement.id = "hover--active";
    }
  };

  const handleMouseLeave = ({ event }) => {
    event.currentTarget.parentElement.id = "";
  };

  const findEvent = (name) => treeNodeContextMenu.find((item) => item && item.text === name);

  useHotkeys(
    "ctrl+del, cmd+backspace",
    () => {
      const deleteEvent = findEvent("Delete");
      deleteEvent.onClick(selectNode);
    },
    [treeNodeContextMenu, selectNode]
  );

  useHotkeys(
    "ctrl+x, cmd+x",
    () => {
      setMoveNode(selectNode);
      setCopyNode(null);
      setIsCopy(false);
    },
    [treeNodeContextMenu, selectNode, copyNode, moveNode]
  );

  useHotkeys(
    "ctrl+c, cmd+c",
    () => {
      setCopyNode(selectNode);
      setMoveNode(null);
      setIsCopy(true);
    },
    [treeNodeContextMenu, selectNode, copyNode, selectedKeys, moveNode]
  );

  useHotkeys(
    "ctrl+v, cmd+v",
    () => {
      if (copyNode && !copyNode.root && selectNode) {
        handleDrop({ node: selectNode, dragNode: copyNode });
      }
      if (moveNode && !moveNode.root && selectNode) {
        handleDrop({ node: selectNode, dragNode: moveNode });
      }
    },
    [treeNodeContextMenu, copyNode, selectNode, expandedKeys, moveNode]
  );

  return (
    <div
      className="tree-wrap animation"
      onClick={rootClick}
      onContextMenu={handleEmptyTreeContextMenu}
    >
      <Tree
        draggable
        allowDrop={({ dropPosition }) => dropPosition !== -1}
        onDrop={handleDrop}
        ref={treeRef}
        itemHeight={20}
        icon={renderIcon}
        treeData={treeData}
        dropIndicatorRender={() => null}
        expandedKeys={expandedKeys}
        selectedKeys={selectedKeys}
        autoExpandParent={autoExpandParent}
        switcherIcon={(nodeProps) => renderSwitcherIcon(nodeProps)}
        onRightClick={handleContextMenu}
        onClick={handleClick}
        onExpand={handleExpand}
        onSelect={handleSelect}
        onDragStart={handleDragStart}
        onDragEnter={handleDragEnter}
        onDragOver={handleDragOver}
        onDragEnd={handleDragEnd}
        onMouseEnter={handleMouseEnter}
        onMouseLeave={handleMouseLeave}
      />
      <Menu animation={false} id="file-tree">
        {renderMenu(treeNodeContextMenu)}
      </Menu>
    </div>
  );
});

export default FileTree;

FileTree.propTypes = {
  projectManager: PropTypes.object,
  onSelect: PropTypes.func,
  initialPath: PropTypes.string,
  contextMenu: PropTypes.object,
  readOnly: PropTypes.func,
};

// TOOD: refactor the dir contruct of the service
export { default as ClipBoardService } from "./clipboard";
