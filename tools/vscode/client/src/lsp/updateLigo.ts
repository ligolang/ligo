import { execFileSync } from 'child_process'

import * as axios from 'axios'
import * as fs from 'fs'
import * as os from 'os'
import * as path from 'path'
import * as semver from 'semver'
import * as vscode from 'vscode'
import {
  LanguageClient,
} from 'vscode-languageclient/node'
import { extensionName } from './common'
import { ligoBinaryInfo, getBinaryPath } from '../common/config'
import { Maybe, isDefined } from '../common/base'

import detectInstaller from 'detect-installer'
import { Readable } from 'stream'
import { NoReleasesAccess } from '../common/exceptions'

/**
 * A download tag name. May be a static Linux binary, or a Windows installer
 * (deprecated).
 */
type TagName = 'Static Linux binary' | 'Ligo Windows installer'

/* eslint-disable camelcase */
/**
 * Stripped version of the release type returned by GitLab with fields that are
 * interesting to us.
 */
type Release = {
  name: string
  tag_name: TagName
  released_at: string
  assets: {
    links: [
      {
        name: string
        direct_asset_url: string
      }
    ]
  }
}
/* eslint-enable camelcase */

/**
 * Stop LIGO, wait for 2 seconds, perform the provided action, then start LIGO
 * again. The action could possibly fail with ETXTBSY otherwise.
 */
async function withClientRestart<T>(client: LanguageClient, action: () => T): Promise<T> {
  if (client.isRunning()) {
    await client.stop()
  }
  const clientStopWaitTime = 2000
  await new Promise(resolve => setTimeout(resolve, clientStopWaitTime))
  let result = action()
  await client.start()
  return result
}

/**
 * Sets the path to the LIGO binary in the VSCode workspace configuration to the
 * provided path.
 */
async function updateLigoPath(path: string): Promise<void> {
  await vscode.workspace.getConfiguration().update(
    ligoBinaryInfo.path,
    path,
    vscode.ConfigurationTarget.Global,
    true,
  )
}

/**
 * Stops the LIGO Language Server, copies the provided binary to the given path,
 * and starts it again. Informs the user about what happened in case of success
 * or failure.
 *
 * @param ligoPath The path in which the provided payload will be written to.
 * @param payload The contents of the LIGO binary.
 * @returns A promise resolving to a boolean indicating whether it was able to
 * succesfully write the binary in the given path (`true`) or not (`false`).
 */
async function copyLigoBinary(
  client: LanguageClient,
  ligoPath: string,
  payload: Buffer,
): Promise<boolean> {
  return await withClientRestart(client, async () => {
    try {
      const fileOptions = {
        encoding: 'binary' as BufferEncoding,
      }

      fs.writeFileSync(ligoPath, payload, fileOptions)
      fs.chmodSync(ligoPath, 0o755)
      await updateLigoPath(ligoPath)
      vscode.window.showInformationMessage(
        `LIGO installed at: ${path.resolve(ligoPath)}. Please restart VS Code to use LIGO LSP.`,
      )
      return true
    } catch (err) {
      vscode.window.showErrorMessage(`Could not install LIGO: ${err.message}`)
      return false
    }
  })
}

/**
 * Queries the GitLab releases page to download LIGO, displaying the progress to
 * the user.
 *
 * @param latest The result of querying the GitLab releases page for the latest
 * LIGO release.
 * @param targetAsset Whether to download a static Linux binary or the Windows
 * installer (deprecated).
 * @returns A promise resolving to the downloaded LIGO binary, or `undefined` if
 * an error occurred.
 */
async function downloadLigo(latest: Release, targetAsset: TagName): Promise<Maybe<Buffer>> {
  const asset = latest.assets.links.find((download) => download.name === targetAsset)
  if (!asset) {
    await vscode.window.showErrorMessage(`Could not find a download for ${targetAsset}.`)
    return undefined
  }

  return await vscode.window.withProgress(
    {
      cancellable: true,
      location: vscode.ProgressLocation.Notification,
      title: `Downloading ${targetAsset}`,
    },
    async (progress, cancelToken) => {
      const controller = new AbortController()
      const response: axios.AxiosResponse<Readable, any> = await axios.default.get(
        asset.direct_asset_url,
        {
          responseType: 'stream',
          signal: controller.signal
        },
      )
      let loaded = 0
      const chunks: Buffer[] = []
      return await new Promise((resolve, reject) => {
        const stream = response.data
        const total = response.headers['content-length']
        stream.on('end', () => resolve(Buffer.concat(chunks)))
        stream.on('error', (err) => reject(err))
        stream.on('data', (chunk) => {
          if (cancelToken.isCancellationRequested) {
            controller.abort()
            vscode.window.showInformationMessage('LIGO download cancelled.')
            reject()
          }

          chunks.push(Buffer.from(chunk))

          loaded += chunk.length
          const percentage = (part: number): number => part * 100. / total
          const increment = percentage(chunk.length)
          progress.report({ increment, message: `${Math.round(percentage(loaded))}%` })
        })
      })
    })
}

/**
 * Queries the GitLab releases page for existing LIGO releases.
 *
 * @returns A promise resolving to a list of available releases, or `undefined`
 * if an error occurred.
 */
async function getLigoReleases(): Promise<Release[] | undefined> {
  // https://stackoverflow.com/a/53126068/10213577
  const ligoGitLabProjectId = 12294987
  const releasesUrl = `https://gitlab.com/api/v4/projects/${ligoGitLabProjectId}/releases/`
  return axios.default
    .get(releasesUrl)
    .then((res) => res.data)
    .catch((err) => {
      vscode.window.showErrorMessage(`Could not fetch LIGO releases: ${err.message}`)
      return undefined
    })
}

/**
 * Queries the GitLab releases page for existing LIGO releases and retuns the
 * most recent release.
 *
 * @returns A promise resolving to a list of available releases, or `undefined`
 * if an error occurred.
 * @throws NoReleasesAccess If we could not fetch releases, or if there is none.
 */
async function getLatestLigoRelease(): Promise<Release> {
  const releases = await getLigoReleases()
  if (!releases || releases.length === 0) {
    throw new NoReleasesAccess()
  }

  return releases[0]
}

/**
 * Opens the user's browser in the LIGO releases page.
 *
 * @returns A promise indicating if the open was successful.
 */
function openLigoReleases(): Thenable<boolean> {
  return vscode.env.openExternal(vscode.Uri.parse('https://gitlab.com/ligolang/ligo/-/releases'))
    .then((result) => {
      if (!result) {
        vscode.window.showErrorMessage('Failed to open LIGO releases page.')
      }

      return result
    })
}

/**
 * Creates a terminal window in VSCode called "LIGO Installer" which will
 * display information about running LIGO's installers.
 */
function mkTerminal(): vscode.Terminal {
  const terminal = vscode.window.createTerminal('LIGO Installer')
  terminal.show(false)
  return terminal
}

/**
 * A template name to create a temporary file in the user's canonical temporary
 * directory.
 */
const ligoTempDownloadTemplate: string = path.join(os.tmpdir(), 'ligo-bin-')

/** Installs LIGO using Homebrew. */
async function runBrewInstaller(client: LanguageClient): Promise<null> {
  const terminal = mkTerminal()
  terminal.sendText(`brew tap ligolang/ligo https://gitlab.com/ligolang/ligo.git`)
  await withClientRestart(client, () => terminal.sendText(`brew install ligolang/ligo/ligo`))
  return null
}

/** Upgrades LIGO using Homebrew. */
async function runBrewUpgrade(client: LanguageClient): Promise<null> {
  const terminal = mkTerminal()
  terminal.sendText(`brew update`)
  await withClientRestart(client, () => terminal.sendText(`brew upgrade ligolang/ligo/ligo`))
  return null
}

/**
 * Installs LIGO using a Windows installer.
 *
 * @deprecated Windows is not supported anymore.
 */
async function runWindowsGuiInstaller(client: LanguageClient, latestRelease: Release): Promise<null> {
  const runAsAdmin = (command: string): void =>
    void execFileSync(
      'powershell',
      [`Start-Process -FilePath ${command} -Verb RunAs -PassThru -Wait`],
    )

  const payload = await downloadLigo(latestRelease, 'Ligo Windows installer').catch(_ => undefined)
  if (!payload) {
    return null
  }

  const showErrorMessage = (err: string) =>
    vscode.window.showErrorMessage(`Error installing LIGO with Windows installer: ${err}`)
  fs.mkdtemp(ligoTempDownloadTemplate, async (err, dir): Promise<void> => {
    if (err) {
      showErrorMessage(err.message)
      return
    }

    const fileOptions = {
      encoding: 'binary' as BufferEncoding,
    }

    const installer = path.join(dir, 'ligo-installer.exe')
    fs.writeFileSync(installer, payload, fileOptions)
    await withClientRestart(client, async () => {
      try {
        runAsAdmin(installer)
        vscode.window.showInformationMessage(`LIGO installed. Please restart VS Code to use LIGO LSP.`)
      } catch (err) {
        showErrorMessage(err.message)
      }
    })
  })
  return null
}

/**
 * Upgrades LIGO using a Windows installer.
 *
 * @deprecated Windows is not supported anymore.
 */
async function runWindowsGuiUpgrade(client: LanguageClient, latestRelease: Release): Promise<null> {
  return runWindowsGuiInstaller(client, latestRelease)
}

/**
 * The npm tag used to categorize LIGO windows releases (deprecated).
 *
 * @deprecated Windows is not supported anymore.
 */
const npmWindowsTag = 'windows'

/**
 * The npm tag used to categorize LIGO macOS releases (Intel CPU architecture).
 */
const npmMacOsIntelTag = 'macos-intel'

/**
 * The npm tag used to categorize LIGO macOS releases (M1 CPU architecture).
 */
const npmMacOsM1Tag = 'macos-m1'

/**
 * Attempts to determine whether to use M1 or Intel architecture using
 * `uname -m`. Defaults to Intel if an exception occurred.
 */
function getNpmMacOsTag(): 'macos-m1' | 'macos-intel' {
  try {
    const arch = execFileSync('uname', ['-m']).toString().trim()
    switch (arch) {
      case 'arm64':
        return npmMacOsM1Tag
      case 'x86_64':
      default:
        return npmMacOsIntelTag
    }
  } catch {
    return npmMacOsIntelTag
  }
}

/**
 * Helper function to handle a LIGO install or upgrade using npm.
 *
 * @see {@link runNpmInstaller}
 * @see {@link runNpmUpgrade}
 */
async function runNpmImpl(client: LanguageClient, platform: NodeJS.Platform, useYarn: boolean, isInstall: boolean): Promise<null> {
  const command =
    useYarn
      ? `yarn global ${isInstall ? 'add' : 'upgrade'}`
      : `npm ${isInstall ? 'install' : 'update'} --global`

  async function run(tag: string): Promise<void> {
    const terminal = mkTerminal()
    // Examples:
    // yarn global add ligolang@macos-intel
    // npm update --global ligolang
    return await withClientRestart(
      client,
      () => terminal.sendText(`${command} ligolang${isInstall ? `@${tag}` : ``}`),
    )
  }

  if (platform === 'win32') {
    run(npmWindowsTag)
  } else if (platform === 'darwin') {
    run(getNpmMacOsTag())
  } else {
    const name = useYarn ? 'Yarn' : 'NPM'
    const procedure = isInstall ? 'install' : 'upgrade'
    vscode.window.showErrorMessage(`Unsupported platform ${platform} for ${name} ${procedure}.`)
  }
  return null
}

/** Installs LIGO using npm. */
async function runNpmInstaller(client: LanguageClient, platform: NodeJS.Platform, useYarn: boolean): Promise<null> {
  return runNpmImpl(client, platform, useYarn, true)
}

/** Upgrades LIGO using npm. */
async function runNpmUpgrade(client: LanguageClient, platform: NodeJS.Platform, useYarn: boolean): Promise<null> {
  return runNpmImpl(client, platform, useYarn, false)
}

/** Installs LIGO from the AUR. */
async function runPacmanInstaller(client: LanguageClient): Promise<null> {
  fs.mkdtemp(ligoTempDownloadTemplate, async (err, dir): Promise<void> => {
    if (err) {
      vscode.window.showErrorMessage(`Error installing LIGO with pacman: ${err.message}`)
      return
    }

    const terminal = mkTerminal()
    terminal.sendText(`git clone https://aur.archlinux.org/ligo-bin.git ${dir}`)
    terminal.sendText(`cd ${dir}`)
    await withClientRestart(client, () => terminal.sendText(`makepkg --syncdeps --install`))
  })
  return null
}

/** Upgrades LIGO from the AUR. */
async function runPacmanUpgrade(client: LanguageClient): Promise<null> {
  return runPacmanInstaller(client)
}

/**
 * Installs LIGO from a static Linux binary, or from the Windows installer
 * (deprecated).
 *
 * @returns A promise resolving to whether the static Linux binary or the
 * Windows installer (deprecated) will be used.
 */
async function runStaticLinuxBinaryInstaller(
  client: LanguageClient,
  ligoPath: Maybe<string>,
  latestRelease: Release
): Promise<TagName | null> {
  const payload = await downloadLigo(latestRelease, 'Static Linux binary').catch(_ => undefined)
  if (!payload) {
    return null
  }

  if (!ligoPath) {
    const uris = await vscode.window.showOpenDialog({
      title: 'Directory to install LIGO',
      openLabel: 'Select',
      canSelectMany: false,
      canSelectFiles: false,
      canSelectFolders: true,
    })
    if (!uris || uris.length === 0) {
      vscode.window.showErrorMessage('LIGO install cancelled')
      return null
    }

    ligoPath = path.join(uris[0].fsPath, 'ligo')
  }

  if (await copyLigoBinary(client, ligoPath, payload)) {
    return latestRelease.tag_name
  }
  return null
}

/**
 * Upgrades LIGO from a static Linux binary, or from the Windows installer
 * (deprecated).
 *
 * @returns A promise resolving to whether the static Linux binary or the
 * Windows installer (deprecated) will be used.
 */
async function runStaticLinuxBinaryUpgrade(
  client: LanguageClient,
  ligoPath: string,
  latestRelease: Release
): Promise<TagName | null> {
  return runStaticLinuxBinaryInstaller(client, ligoPath, latestRelease)
}

/** How to install LIGO in Linux. */
type LinuxInstallMethod = "npm" | "pacman" | "yarn" | null

/** How to install LIGO in macOS. */
type MacOSInstallMethod = "brew" | "npm" | "yarn" | null

/**
 * How to install LIGO in Windows.
 *
 * @deprecated Windows is not supported anymore.
 */
type WindowsInstallMethod = "GUI Installer" | "npm" | "yarn" | null

/** How to install LIGO on the current OS. */
type InstallMethod = LinuxInstallMethod | MacOSInstallMethod | WindowsInstallMethod

/** How to upgrade LIGO on the current OS. */
type ChosenUpgradeMethod = 'Static Binary' | 'Upgrade' | 'Open Downloads' | 'Cancel'

/**
 * How to install LIGO on the current OS, or an option to allow the user to set
 * a path to LIGO.
 */
type ChosenInstallMethod = 'Static Binary' | 'GUI installer' | 'NPM' | 'Yarn' | 'Homebrew' | 'AUR' | 'Open Downloads' | 'Cancel' | 'Choose path'

/**
 * Shows an error message to the user asking them to choose a path to LIGO, or
 * how to install LIGO.
 */
async function askUserToInstall(platform: NodeJS.Platform, message: string): Promise<ChosenInstallMethod> {
  let chosen = await vscode.window.showErrorMessage(message, 'Choose path', 'Install...', 'Cancel')
  switch (chosen) {
    case 'Choose path':
      return 'Choose path'
    case 'Cancel':
    case undefined:
      return 'Cancel'
    case 'Install...':
      return await askUserInstallDetails(platform)
  }
}

/**
 * Shows an error message to the user asking them to choose a path to LIGO, or
 * how to install LIGO.
 */
async function askUserInstallDetails(platform: NodeJS.Platform): Promise<ChosenInstallMethod> {
  let options: ({ type: ChosenInstallMethod, detail?: string })[] =
    [{
      type: 'Open Downloads',
      detail: 'Open releases page on LIGO GitLab.'
    }]

  switch (platform) {
    case 'win32':
      options.push(
        {
          type: 'GUI installer',
          detail: 'Download and run official installation wizard.'
        }
      )
      break
    case 'linux':
      // TODO: we may as well suggest the debian package
      options.push(
        {
          type: 'Static Binary',
          detail: 'Download and install the official static binary distribution.'
        },
        {
          type: 'AUR',
          detail: 'Install using pacman.'
        }
      )
      break
    case 'darwin':
      options.push(
        { type: 'NPM' },
        { type: 'Yarn' },
        { type: 'Homebrew' }
      )
      break
  }

  // In options, copy `type` field to `label` field.
  // Note that 'label' is a mere string (not enum), so we have to keep both.
  let items: (vscode.QuickPickItem & { type: ChosenInstallMethod })[] =
    options.map(o => ({ type: o.type, label: o.type, detail: o.detail }))
  let quickPickOptions = { title: "Choose LIGO installation method", ignoreFocusOut: true }
  let result = await vscode.window.showQuickPick(items, quickPickOptions)

  if (isDefined(result)) {
    return result.type
  } else {
    return 'Cancel'
  }
}

/**
 * Asks the user to upgrade their LIGO install. Attempts to detect the OS and
 * install method to suggest an appropriate upgrading method.
 */
async function askUserToUpgrade(platform: NodeJS.Platform, installer: InstallMethod, message: string): Promise<Maybe<ChosenUpgradeMethod>> {
  switch (installer) {
    case 'GUI Installer':
      return await vscode.window.showInformationMessage(
        `${message} Let ${extensionName} download and run LIGO's installer to upgrade?`,
        'Upgrade',
        'Open Downloads',
        'Cancel',
      )
    case 'brew':
    case 'npm':
    case 'pacman':
    case 'yarn':
      return await vscode.window.showInformationMessage(
        `${message} Let ${extensionName} run ${installer} to upgrade?`,
        'Upgrade',
        'Open Downloads',
        'Cancel',
      )
    default:
      if (platform === 'linux') {
        return await vscode.window.showInformationMessage(
          `${message} If you use the static Linux binary, please select "Static Binary", otherwise "Open Downloads".`,
          'Static Binary',
          'Open Downloads',
          'Cancel',
        )
      } else {
        return await vscode.window.showInformationMessage(
          `${message} Please consider upgrading it.`,
          'Open Downloads',
          'Cancel',
        )
      }
  }
}

/** Runs the installer for the chosen install method. */
async function runInstaller(
  client: LanguageClient,
  platform: NodeJS.Platform,
  answer: Maybe<ChosenInstallMethod>,
): Promise<boolean> {
  switch (answer) {
    case 'Static Binary': {
      const latestRelease = await getLatestLigoRelease()
      return !!await runStaticLinuxBinaryInstaller(client, undefined, latestRelease)
    }
    case 'GUI installer': {
      const latestRelease = await getLatestLigoRelease()
      return !!await runWindowsGuiInstaller(client, latestRelease)
    }
    case 'AUR':
      return !!await runPacmanInstaller(client)
    case 'NPM':
      return !!await runNpmInstaller(client, platform, false)
    case 'Yarn':
      return !!await runNpmInstaller(client, platform, true)
    case 'Homebrew':
      return !!await runBrewInstaller(client)
    case 'Choose path': {
      const uris = await vscode.window.showOpenDialog({
        title: 'Path to LIGO',
        openLabel: 'Select',
        canSelectMany: false,
      })
      if (!uris || uris.length === 0) {
        return false
      }

      await updateLigoPath(uris[0].fsPath)
      await updateLigo(client)
      return true
    }
    case 'Open Downloads':
      openLigoReleases()
      return false
    case 'Cancel':
    default:
      return false
  }
}

/** Runs the installer for the chosen upgrade method. */
async function runUpgrade(
  client: LanguageClient,
  ligoPath: string,
  latestRelease: Release,
  platform: NodeJS.Platform,
  installer: InstallMethod,
  answer: Maybe<ChosenUpgradeMethod>,
): Promise<string | null> {
  switch (answer) {
    case 'Static Binary': return await runStaticLinuxBinaryUpgrade(client, ligoPath, latestRelease)
    case 'Upgrade':
      switch (installer) {
        case 'brew': return await runBrewUpgrade(client)
        case 'GUI Installer': return await runWindowsGuiUpgrade(client, latestRelease)
        case 'npm': return await runNpmUpgrade(client, platform, false)
        case 'pacman': return await runPacmanUpgrade(client)
        case 'yarn': return await runNpmUpgrade(client, platform, true)
        default: return null
      }
    case 'Open Downloads':
      openLigoReleases()
      return null
    case 'Cancel':
    default:
      return null
  }
}

/**
 * Checks the OS and installation method for LIGO and asks the user to upgrade
 * to a newer version.
 */
async function promptLigoUpdate(
  client: LanguageClient,
  ligoPath: string,
  installedVersionIdentifier: string | number,
): Promise<string | number> {
  const latestRelease = await getLatestLigoRelease()

  switch (typeof installedVersionIdentifier) {
    // Semantic version
    case 'string':
      if (semver.eq(installedVersionIdentifier, latestRelease.tag_name)) {
        return installedVersionIdentifier
      }
      break
    // Date of some rolling release
    case 'number':
      if (new Date(installedVersionIdentifier) >= new Date(latestRelease.released_at)) {
        return installedVersionIdentifier
      }
      break
    default:
      vscode.window.showErrorMessage(`Unknown version: ${installedVersionIdentifier}`)
  }

  const installer: InstallMethod = await detectInstaller({ cwd: process.cwd() }).catch((_: any) => null)
  const platform: NodeJS.Platform = os.platform()
  const answer = await askUserToUpgrade(platform, installer, 'A new LIGO version is available.')
  const tagName = await runUpgrade(client, ligoPath, latestRelease, platform, installer, answer)

  return tagName || installedVersionIdentifier
}

/**
 * Handles a LIGO upgrade or install, depending on whether `suggestUpdate` is
 * `true` or `false`, respectively.
 */
async function runUpdateOrInstall(
  client: LanguageClient,
  errorMessage: string,
  suggestUpdate: boolean,
  ligoPath: string,
): Promise<boolean> {
  const platform = os.platform()
  const installer = await detectInstaller({ cwd: process.cwd() }).catch((_: any) => null)

  if (suggestUpdate) {
    const latestRelease = getLatestLigoRelease()
    const answer: Maybe<ChosenUpgradeMethod> = await askUserToUpgrade(platform, installer, errorMessage)
    return !!await runUpgrade(client, ligoPath, await latestRelease, platform, installer, answer)
  } else {
    const answer: ChosenInstallMethod = await askUserToInstall(platform, errorMessage)
    return await runInstaller(client, platform, answer)
  }
}

/**
 * Handles a LIGO upgrade, suggesting the user to install if not possible.
 */
export default async function updateLigo(client: LanguageClient): Promise<void> {
  let ligoPath: string = getBinaryPath(ligoBinaryInfo)
  try {
    await updateLigoUnchecked(client, ligoPath)
  } catch (err) {
    const isLikelyNotFoundError = /ENOENT/.test(err.message)
    const isLikelyPermissionDeniedError = /EACCES/.test(err.message)

    let hint = ''
    if (isLikelyNotFoundError) {
      hint = `\nHint: Ensure that you've specified the path to LIGO in your Visual Studio Code settings and you have LIGO with support for \`ligo lsp\`, starting from version 0.61.0.`
    }
    if (isLikelyPermissionDeniedError) {
      hint = '\nHint: Check the file permissions for LIGO.'
    }

    await runUpdateOrInstall(
      client,
      `Could not find a LIGO installation on your computer or the installation is invalid. Choose path to the LIGO executable or consider using one of the available installation options. Details: ${err.message}. ${hint}`,
      false,
      ligoPath,
    )
  }
}

/**
 * Checks the user's LIGO version and the latest release's version, suggesting
 * an update if there is a mismatch.
 */
async function updateLigoUnchecked(client: LanguageClient, ligoPath: string): Promise<void> {
  let data: string = execFileSync(ligoPath, ['--version']).toString().trim()

  if (data === '') {
    // We purposefully omit the `await` here to avoid the extension hanging.
    vscode.window.showWarningMessage(
      '`ligo --version` returned the empty string. Assuming it was built locally. Ensure this LIGO build supports `ligo lsp`.',
    )
    return
  }

  async function unsupportedVersion<T>(): Promise<T> {
    await runUpdateOrInstall(
      client,
      'You need LIGO version 0.61.0 or newer so that `ligo lsp` may work. Closing the language server. Please update and try again.',
      true,
      ligoPath,
    )
    throw new Error("Unsupported version")
  }

  async function validateSemver(version: string): Promise<void> {
    const semverTest = semver.valid(semver.coerce(version))
    if (semverTest) {
      const newVersion = await promptLigoUpdate(client, ligoPath, semverTest)
      switch (typeof newVersion) {
        case 'string':
          if (semver.lt(newVersion, '1.0.0')) {
            vscode.window.showInformationMessage(
              'LIGO v1 is released. Please visit https://ligolang.org for more information.',
            )
          }
          if (semver.lt(newVersion, '0.61.0')) {
            return await unsupportedVersion()
          }
          break
        case 'number':
          validateRollingRelease(version)
          break
      }
    }
  }

  async function validateRollingRelease(version: string): Promise<void> {
    const commitTest =
      /Rolling release\nCommit SHA: [0-9a-f]{40}\nCommit Date: ([^\n]+)|[0-9a-f]{40}\n([^\n]+)/
    const commitDate = commitTest.exec(version)
    if (commitDate && commitDate.length === 3) {
      const date: number = Date.parse(commitDate[1] || commitDate[2])
      if (Number.isNaN(date)) {
        // Parse failed; not a date.
        return
      }

      const newVersion = promptLigoUpdate(client, ligoPath, date)
      switch (typeof newVersion) {
        case 'string':
          validateSemver(version)
          break;
        case 'number':
          // LIGO 0.61.0 release date
          // Note: month is 0-indexed
          if (newVersion < Date.UTC(2023, 3 - 1, 1)) {
            return await unsupportedVersion()
          }
          break
      }
    } else {
      vscode.window.showErrorMessage(
        `Could not identify the installed LIGO version: ${version}. Ensure this LIGO build supports \`ligo lsp\`.`,
      )
    }
  }

  await validateSemver(data)
}
