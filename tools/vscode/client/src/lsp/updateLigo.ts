import { execFileSync } from 'child_process'

import * as axios from 'axios'
import * as fs from 'fs/promises'
import * as os from 'os'
import * as path from 'path'
import * as semver from 'semver'
import * as vscode from 'vscode'
import * as cp from 'child_process'
import * as util from 'util'
import {
  LanguageClient,
} from 'vscode-languageclient/node'
import { extensionName } from './common'
import { ligoBinaryInfo, getBinaryPath } from '../common/config'
import { Maybe, isDefined } from '../common/base'

import { Readable } from 'stream'
import { NoReleasesAccess, UnsupportedPlatform, HomebrewError, PacmanError } from '../common/exceptions'

/**
 * A download asset name. May be a static Linux binary, deb package or ARM64 binary.
 * 
 * @see https://gitlab.com/api/v4/projects/12294987/releases/
 */
type AssetName = 'Static Linux binary' | 'deb package' | 'ligo ARM64 Unix'

/* eslint-disable camelcase */
/**
 * Stripped version of the release type returned by GitLab with fields that are
 * interesting to us.
 * 
 * @see https://gitlab.com/api/v4/projects/12294987/releases/ for example data.
 */
type Release = {
  name: string
  tag_name: string
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

      await fs.writeFile(ligoPath, payload, fileOptions)
      await fs.chmod(ligoPath, 0o755)
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
async function downloadLigo(latest: Release, targetAsset: AssetName): Promise<Maybe<Buffer>> {
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
 * Creates a terminal window in VSCode called "LIGO Installer" which will
 * display information about running LIGO's installers.
 */
function mkTerminal(): vscode.Terminal {
  const terminal = vscode.window.createTerminal('LIGO Installer')
  terminal.show(false)
  return terminal
}

/** How to install LIGO in Linux. */
type LinuxInstallMethod = "pacman" | null

/** How to install LIGO in macOS. */
type MacOSInstallMethod = "brew" | null

/** How to install LIGO on the current OS. */
type InstallMethod = LinuxInstallMethod | MacOSInstallMethod

/** 
 * Locates the first file in the provided list that exists.
 * 
 * @returns The first file that exists, or `undefined` if none exist.
 */
async function locateFile(paths: Iterable<string>): Promise<Maybe<string>> {
  const cwd = process.cwd()

  for (const path_ of paths) {
    try {
      // FIXME: Check if this throws an error if the file does not exist.
      const stat = await fs.stat(path.resolve(cwd, path_));

      if (stat.isFile()) {
        return path_;
      }
    } catch { }
  }
}


async function locateBinary(binary: string): Promise<Maybe<string>> {
  const envPath = process.env.PATH
  if (!envPath) {
    return
  }

  const paths = envPath.split(path.delimiter)
  const binaries = paths.map(p => path.join(p, binary))

  return await locateFile(binaries)
}

const LIGO_BINARY_NAME = 'ligo'
const cpExec = util.promisify(cp.exec)

async function isLigoInstalledByBrew(): Promise<boolean> {
  try {
    const { stdout, stderr } = await cpExec('brew list -1 --formulae');

    if (stderr != "") {
      throw new HomebrewError('Could not list installed formulae.', { stderr })
    }

    if (stdout == "") {
      throw new HomebrewError('`brew list` returned empty output')
    }

    return stdout.includes(LIGO_BINARY_NAME)
  } catch {
    throw new HomebrewError('Could not list installed formulae.')
  }
}

async function isLigoInstalledByPacman(): Promise<boolean> {
  try {
    const { stdout, stderr } = await cpExec(`pacman -Qs ${LIGO_BINARY_NAME}`);

    if (stderr != "") {
      throw new PacmanError('Could not query installed packages.', { stderr })
    }

    if (stdout == "") {
      throw new PacmanError('`pacman -Qs` returned empty output')
    }

    return true
  } catch {
    throw new PacmanError('Could not query installed packages.')
  }
}

async function detectMacosIntaller(): Promise<MacOSInstallMethod> {
  const brew = await locateBinary('brew')
  if (brew && await isLigoInstalledByBrew()) {
    return 'brew'
  }

  return null
}

async function detectLinuxInstaller(): Promise<LinuxInstallMethod> {
  const pacman = await locateBinary('pacman')
  if (pacman && await isLigoInstalledByPacman()) {
    return 'pacman'
  }

  return null
}

async function detectInstaller(platform: NodeJS.Platform): Promise<InstallMethod> {
  switch (platform) {
    case "darwin":
      return await detectMacosIntaller()
    case "linux":
      return await detectLinuxInstaller()
    default:
      throw new UnsupportedPlatform(platform)
  }
}

/** Installs LIGO using Homebrew. */
async function runBrewInstaller(client: LanguageClient): Promise<boolean> {
  const terminal = mkTerminal()
  terminal.sendText(`brew tap ligolang/ligo https://gitlab.com/ligolang/ligo.git`)
  await withClientRestart(client, () => terminal.sendText(`brew install ligolang/ligo/ligo`))
  return true
}

/** Upgrades LIGO using Homebrew. */
async function runBrewUpgrade(client: LanguageClient): Promise<boolean> {
  const terminal = mkTerminal()
  terminal.sendText(`brew update`)
  await withClientRestart(client, () => terminal.sendText(`brew upgrade ligolang/ligo/ligo`))
  return true
}

/**
 * A template name to create a temporary file in the user's canonical temporary
 * directory.
 */
const ligoTempDownloadTemplate: string = path.join(os.tmpdir(), 'ligo-bin-')

/** Installs LIGO from the AUR. */
async function runPacmanInstaller(client: LanguageClient): Promise<boolean> {
  try {
    const dir = await fs.mkdtemp(ligoTempDownloadTemplate)
    const terminal = mkTerminal()
    terminal.sendText(`git clone https://aur.archlinux.org/ligo-bin.git ${dir}`)
    terminal.sendText(`cd ${dir}`)
    await withClientRestart(client, () => terminal.sendText(`makepkg --syncdeps --install`))
    return true
  } catch (err) {
    vscode.window.showErrorMessage(`Error installing LIGO with pacman: ${err.message}`)
    return false
  }
}

/** Upgrades LIGO from the AUR. */
async function runPacmanUpgrade(client: LanguageClient): Promise<boolean> {
  return runPacmanInstaller(client)
}

/**
 * Installs LIGO from a static Linux binary
 *
 * @returns A promise resolving to `true` if the installation was successful
 */
async function runStaticLinuxBinaryInstaller(
  client: LanguageClient,
  ligoPath: Maybe<string>,
  latest: Release
): Promise<boolean> {
  const payload = await downloadLigo(latest, 'Static Linux binary').catch(_ => undefined)
  if (!payload) {
    return false
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
      return false
    }

    ligoPath = path.join(uris[0].fsPath, 'ligo')
  }

  if (await copyLigoBinary(client, ligoPath, payload)) {
    return true
  }

  return false
}

/**
 * Upgrades LIGO from a static Linux binary
 *
 * @returns A promise resolving to `true` if the upgrade was successful
 */
async function runStaticLinuxBinaryUpgrade(
  client: LanguageClient,
  ligoPath: string,
  latest: Release
): Promise<boolean> {
  return runStaticLinuxBinaryInstaller(client, ligoPath, latest)
}

/** How to upgrade LIGO on the current OS. */
type ChosenUpgradeMethod = 'Static Binary' | 'Upgrade' | 'Open Downloads' | 'Cancel'

/**
 * How to install LIGO on the current OS, or an option to allow the user to set
 * a path to LIGO.
 */
type ChosenInstallMethod = 'Static Binary' | 'Homebrew' | 'AUR' | 'Open Downloads' | 'Cancel' | 'Choose path'

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
    case 'brew':
    case 'pacman':
      return await vscode.window.showInformationMessage(
        `${message} Let ${extensionName} run ${installer} to upgrade?`,
        'Upgrade',
        'Open Downloads',
        'Cancel',
      )
    default:
      // TODO: support static binary for macOS (arm64 unix)
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
  answer: Maybe<ChosenInstallMethod>,
): Promise<boolean> {
  switch (answer) {
    case 'Static Binary': {
      const latestRelease = await getLatestLigoRelease()
      return await runStaticLinuxBinaryInstaller(client, undefined, latestRelease)
    }
    case 'AUR':
      return await runPacmanInstaller(client)
    case 'Homebrew':
      return await runBrewInstaller(client)
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
  installer: InstallMethod,
  answer: Maybe<ChosenUpgradeMethod>,
): Promise<boolean> {
  switch (answer) {
    case 'Static Binary': return await runStaticLinuxBinaryUpgrade(client, ligoPath, latestRelease)
    case 'Upgrade':
      switch (installer) {
        case 'brew': return await runBrewUpgrade(client)
        case 'pacman': return await runPacmanUpgrade(client)
        default: return false
      }
    case 'Open Downloads':
      openLigoReleases()
      return false
    case 'Cancel':
    default:
      return false
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

  const platform: NodeJS.Platform = os.platform()
  const installer: InstallMethod = await detectInstaller(platform).catch((_: any) => null)
  const answer = await askUserToUpgrade(platform, installer, 'A new LIGO version is available.')
  if (!await runUpgrade(client, ligoPath, latestRelease, installer, answer)) {
    return installedVersionIdentifier
  }

  return latestRelease.tag_name
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

  if (suggestUpdate) {
    const installer = await detectInstaller(platform).catch((_: any) => null)
    const answer: Maybe<ChosenUpgradeMethod> = await askUserToUpgrade(platform, installer, errorMessage)
    const latestRelease = await getLatestLigoRelease()
    return await runUpgrade(client, ligoPath, latestRelease, installer, answer)
  } else {
    const answer: ChosenInstallMethod = await askUserToInstall(platform, errorMessage)
    return await runInstaller(client, answer)
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
