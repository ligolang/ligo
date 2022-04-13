import { IpcChannel } from '@obsidians/ipc'
import fileOps from '@obsidians/file-ops'

export default function initTerminalChannel (logId, cwd = fileOps.current.workspace) {
  const terminalCreator = new IpcChannel('terminal-creator')
  terminalCreator.invoke('create', logId, cwd)
  return new IpcChannel('terminal', logId)
}
