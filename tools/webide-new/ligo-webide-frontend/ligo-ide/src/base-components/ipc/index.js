const TempIpcChannel = require("./HttpIpcChannel").default;

export const IpcChannel = TempIpcChannel;

export { default as HttpIpcChannel } from "./HttpIpcChannel";
