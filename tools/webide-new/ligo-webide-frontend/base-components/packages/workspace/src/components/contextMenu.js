import platform from '@obsidians/platform'
import fileOps from '@obsidians/file-ops'
import { ClipBoardService } from '@obsidians/filetree';
const handlers = {}

const showInFinder = node => {
  if (node.root) {
    fileOps.current.openItem(node.path)
  } else {
    fileOps.current.showItemInFolder(node.path)
  }
}

const copyPath = ({ path, pathInProject }) => {
  const filePath = pathInProject || path
  const clipboard = new ClipBoardService()

  clipboard.writeText(filePath)
}

const openInTerminal = node => {
  const basePath = node.children ? node.path : fileOps.current.path.dirname(node.path)
  fileOps.current.openInTerminal(basePath)
}

export const registerHandlers = ({ newFile, newFolder, rename, deleteFile, openFile }) => {
  handlers.newFile = newFile
  handlers.newFolder = newFolder
  handlers.rename = rename
  handlers.deleteFile = deleteFile
  handlers.openFile = openFile
}

let contextMenu
if (platform.isDesktop) {
  contextMenu = [
    { text: 'New File', onClick: node => handlers.newFile(node) },
    { text: 'New Folder', onClick: node => handlers.newFolder(node) },
    null,
    { text: 'Open', onClick: node => handlers.openFile(node) },
    null,
    { text: 'Open Containing Folder', onClick: showInFinder },
    { text: 'Open in Terminal', onClick: openInTerminal },
    null,
    { text: 'Copy Path', onClick: copyPath },
    null,
    { text: 'Rename', onClick: node => handlers.rename(node) },
    { text: 'Delete', onClick: node => handlers.deleteFile(node) },
  ]
} else {
  contextMenu = [
    { text: 'New File', onClick: node => handlers.newFile(node) },
    { text: 'New Folder', onClick: node => handlers.newFolder(node) },
    // null,
    // { text: 'Download' },
    null,
    { text: 'Copy Path', onClick: copyPath },
    null,
    { text: 'Rename', onClick: node => handlers.rename(node) },
    { text: 'Delete', onClick: node => handlers.deleteFile(node) },
  ]
}

export default contextMenu