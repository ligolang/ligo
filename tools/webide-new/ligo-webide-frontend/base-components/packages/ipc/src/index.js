let TempIpcChannel = require('./HttpIpcChannel').default

export const IpcChannel = TempIpcChannel

export { default as IoIpcChannel } from './IoIpcChannel'
export { default as HttpIpcChannel } from './HttpIpcChannel'
export { default as BuildService } from './HttpIpcChannel/BuildService'
