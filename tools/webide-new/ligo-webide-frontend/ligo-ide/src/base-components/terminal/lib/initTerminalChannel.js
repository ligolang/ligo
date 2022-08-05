import { IpcChannel } from "~/base-components/ipc";
import fileOps from "~/base-components/file-ops";

export default function initTerminalChannel(logId, cwd = fileOps.workspace) {
  const terminalCreator = new IpcChannel("terminal-creator");
  terminalCreator.invoke("create", logId, cwd);
  return new IpcChannel("terminal", logId);
}
