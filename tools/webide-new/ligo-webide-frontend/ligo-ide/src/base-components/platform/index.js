let type, os

type = 'web'
const appVersion = navigator.appVersion
if (appVersion.indexOf('Win') > -1) {
  os = 'win'
} else if (appVersion.indexOf('Mac') > -1) {
  os = 'mac'
} else if (appVersion.indexOf('Linux') > -1) {
  os = 'linux'
}

export default {
  get type () { return type },
  get isDesktop () { return type === 'desktop' },
  get isWeb () { return type === 'web' },
  get os () { return os }
}
