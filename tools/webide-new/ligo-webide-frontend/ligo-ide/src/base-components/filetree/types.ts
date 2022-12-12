export type RefreshNewData = {
  type: "newFile" | "newDirectory";
  name: string;
  path: string;
  basePath: string;
};

export type RefreshRenameData = {
  type: "renameFile" | "renameDirectory";
  newName: string;
  newPath: string;
  oldPath: string;
};

export type RefreshDeleteData = {
  type: "deleteFile" | "deleteDirectory";
  path: string;
};

export type RefreshCopyMoveData = {
  type: "moveFile" | "moveDirectory" | "copyFile" | "copyDirectory";
  targetPath: string;
  dropPath: string;
};

export type RefreshData =
  | RefreshNewData
  | RefreshRenameData
  | RefreshDeleteData
  | RefreshCopyMoveData;
