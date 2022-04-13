let type, os, appleSilicon

function isElectron() {
  // Renderer process
  if (typeof window !== 'undefined' && typeof window.process === 'object' && window.process.type === 'renderer') {
      return true;
  }

  // Main process
  if (typeof process !== 'undefined' && typeof process.versions === 'object' && !!process.versions.electron) {
      return true;
  }

  // Detect the user agent when the `nodeIntegration` option is set to true
  if (typeof navigator === 'object' && typeof navigator.userAgent === 'string' && navigator.userAgent.indexOf('Electron') >= 0) {
      return true;
  }

  return false;
}

if (isElectron()) {
  type = 'desktop'
  const { OS_IS_WINDOWS, OS_IS_MAC, OS_IS_LINUX } = process.env
  const nodemodule_os = require('os')
  appleSilicon = Boolean(nodemodule_os.cpus().find(cpu => cpu.model.startsWith('Apple M')))

  if (OS_IS_WINDOWS) {
    os = 'win'
  } else if (OS_IS_MAC) {
    os = 'mac'
  } else if (OS_IS_LINUX) {
    os = 'linux'
  }
} else {
  type = 'web'
  const appVersion = navigator.appVersion
  if (appVersion.indexOf('Win') > -1) {
    os = 'win'
  } else if (appVersion.indexOf('Mac') > -1) {
    os = 'mac'
  } else if (appVersion.indexOf('Linux') > -1) {
    os = 'linux'
  }
}

export default {
  get type () { return type },
  get isDesktop () { return type === 'desktop' },
  get isWeb () { return type === 'web' },
  get os () { return os },
  get isAppleSilicon () { return appleSilicon }
}
