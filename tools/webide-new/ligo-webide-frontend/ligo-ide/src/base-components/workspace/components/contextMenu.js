import { ClipBoardService } from '~/base-components/filetree'
const handlers = {}

const copyPath = ({ path, pathInProject }) => {
  const filePath = pathInProject || path
  const clipboard = new ClipBoardService()

  clipboard.writeText(filePath)
}

export const registerHandlers = ({ newFile, newFolder, rename, deleteFile, openFile }) => {
  handlers.newFile = newFile
  handlers.newFolder = newFolder
  handlers.rename = rename
  handlers.deleteFile = deleteFile
  handlers.openFile = openFile
}

let contextMenu = [
    { text: 'New File', onClick: node => handlers.newFile(node) },
    { text: 'New Folder', onClick: node => handlers.newFolder(node) },
    // null,
    // { text: 'Download' },
  null,
    { text: 'Copy Path', onClick: copyPath },
  null,
    { text: 'Rename', onClick: node => handlers.rename(node) },
    { text: 'Delete', onClick: node => handlers.deleteFile(node) }
]

export default contextMenu
