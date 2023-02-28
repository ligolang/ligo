import { execFileSync } from 'child_process'

import * as axios from 'axios'
import * as path from 'path'
import * as fs from 'fs'
import * as semver from 'semver'
import * as vscode from 'vscode'
import { getBinaryPath } from './commands/common'

/* eslint-disable camelcase */
/**
 * Stripped version of the release type returned by GitLab with fields that are
 * interesting to us.
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

export async function installLigo(ligoPath: string, latest: Release): Promise<boolean> {
  const asset = latest.assets.links.find((download) => download.name === 'Static Linux binary')
  if (!asset) {
    await vscode.window.showErrorMessage('Could not find a download for a static Linux binary.')
    return false
  }

  const fileOptions = {
    encoding: 'binary' as BufferEncoding,
  }

  return vscode.window.withProgress(
    {
      cancellable: true,
      location: vscode.ProgressLocation.Notification,
      title: 'Downloading static Linux binary for LIGO',
    },
    async (progress, cancelToken) => axios.default
      .get(
        asset.direct_asset_url,
        {
          responseType: 'arraybuffer',
          cancelToken: new axios.default.CancelToken(cancelToken.onCancellationRequested),
          onDownloadProgress: (progressEvent) => {
            const increment = Math.round((progressEvent.loaded / progressEvent.total) * 100)
            const newState = {
              increment,
              message: `${increment}%`,
            }
            progress.report(newState)
          },
        },
      )
      .then((res) => {
        if (cancelToken.isCancellationRequested) {
          vscode.window.showInformationMessage('LIGO installation cancelled.')
          return false
        }

        // FIXME: Sometimes the installation may fail. This will happen if ligo
        // is currently being used by the extension. The error will be ETXTBSY
        // and indicates that the file is currently busy.
        fs.writeFile(ligoPath, Buffer.from(res.data), fileOptions, (err) => {
          if (err) {
            vscode.window.showErrorMessage(`Could not install LIGO: ${err.message}`)
            return false
          } else {
            vscode.window.showInformationMessage(`LIGO installed at: ${path.resolve(ligoPath)}`)
            return true
          }
        })
      })
      .catch((err) => {
        if (axios.default.isCancel(err)) {
          vscode.window.showInformationMessage('LIGO download cancelled.')
        }

        vscode.window.showErrorMessage(`Could not download LIGO: ${err.message}`)
        return false
      }),
  )
}

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

async function getLatestLigoRelease(): Promise<Release | undefined> {
  const releases = await getLigoReleases()
  if (!releases || releases.length === 0) {
    return undefined
  }

  return releases[0]
}

function openLigoReleases(): Thenable<boolean> {
  return vscode.env.openExternal(vscode.Uri.parse('https://gitlab.com/ligolang/ligo/-/releases'))
    .then((result) => {
      if (!result) {
        vscode.window.showErrorMessage('Failed to open LIGO releases page.')
      }

      return result
    })
}

async function promptLigoUpdate(
  ligoPath: string,
  installedVersionIdentifier: string | number,
): Promise<string | number> {
  const latestRelease = await getLatestLigoRelease()
  if (!latestRelease) {
    return
  }

  switch (typeof installedVersionIdentifier) {
    // Semantic version
    case 'string':
      if (semver.gte(installedVersionIdentifier, latestRelease.tag_name)) {
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

  const answer = await vscode.window.showInformationMessage(
    'A new LIGO version is available. If you use the static Linux binary, please select "Static Linux Binary", otherwise "Open Downloads".',
    'Static Linux Binary',
    'Open Downloads',
    'Cancel',
  )

  switch (answer) {
    case 'Static Linux Binary':
      if (await installLigo(ligoPath, latestRelease)) {
        return latestRelease.tag_name
      }
      break
    case 'Open Downloads':
      openLigoReleases()
      break
    case 'Cancel':
    default:
      break
  }

  return installedVersionIdentifier
}

export default async function updateLigo(): Promise<void> {
  const config = vscode.workspace.getConfiguration()
  /* eslint-disable no-use-before-define */
  return updateLigoImpl(config)
  /* eslint-enable no-use-before-define */
}

async function showUpdateError(
  errorMessage: string,
  suggestUpdate: boolean,
  ligoPath: string,
  config: vscode.WorkspaceConfiguration
): Promise<boolean> {
  const answer = await
    (suggestUpdate
      ? vscode.window.showErrorMessage(
        errorMessage,
        'Download static Linux binary',
        'Choose path',
        'Download',
        'Cancel',
      )
      : vscode.window.showErrorMessage(
        errorMessage,
        'Choose path',
        'Download',
        'Cancel',
      ))

  switch (answer) {
    case 'Download static Linux binary':
      const latestRelease = await getLatestLigoRelease()
      return await installLigo(ligoPath, latestRelease)
    case 'Choose path': {
      const uris = await vscode.window.showOpenDialog({ canSelectMany: false })
      if (!uris || uris.length === 0) {
        return false
      }

      await config.update(
        'ligoLanguageServer.ligoBinaryPath',
        uris[0].fsPath,
        vscode.ConfigurationTarget.Global,
        true,
      )
      await updateLigoImpl(config)
      return true
    }
    case 'Download':
      openLigoReleases()
      return false
    case 'Cancel':
    default:
      return false
  }
}

async function updateLigoImpl(config: vscode.WorkspaceConfiguration): Promise<void> {
  const ligoPath = getBinaryPath({ name: 'ligo', path: 'ligoLanguageServer.ligoBinaryPath' }, config)

  let data: string
  try {
    if (!ligoPath) {
      throw new Error('Undefined path')
    }

    data = execFileSync(ligoPath, ['--version']).toString().trim()
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

    const shouldContinue = await showUpdateError(
      `Could not find a LIGO installation on your computer or the installation is invalid: ${err.message}. ${hint}`,
      false,
      ligoPath,
      config,
    )

    if (!shouldContinue) {
      return
    }
  }

  if (data === '') {
    // We purposefully omit the `await` here to avoid the extension hanging.
    vscode.window.showWarningMessage(
      '`ligo --version` returned the empty string. Assuming it was built locally. Ensure this LIGO build supports `ligo lsp`.',
    )
    return
  }

  async function unsupportedVersion<T>(): Promise<T> {
    await showUpdateError(
      'You need LIGO version 0.61.0 or greater so that `ligo lsp` may work. Closing the language server. Please update and try again.',
      true,
      ligoPath,
      config,
    )
    throw new Error("Unsupported version")
  }

  async function validateSemver(version: string) {
    const semverTest = semver.valid(semver.coerce(version))
    if (semverTest) {
      const newVersion = await promptLigoUpdate(ligoPath, semverTest)
      switch (typeof newVersion) {
        case 'string':
          // TODO: Replace with actual LIGO version with LIGO LSP.
          if (semver.lt(newVersion, semver.coerce('0.61.0'))) {
            return await unsupportedVersion()
          }
          break
        case 'number':
          validateRollingRelease(version)
          break
      }
    }
  }

  async function validateRollingRelease(version: string) {
    const commitTest =
      /Rolling release\nCommit SHA: [0-9a-f]{40}\nCommit Date: ([^\n]+)|[0-9a-f]{40}\n([^\n]+)/
    const commitDate = commitTest.exec(version)
    if (commitDate && commitDate.length === 3) {
      const date: number = Date.parse(commitDate[1] || commitDate[2])
      if (Number.isNaN(date)) {
        // Parse failed; not a date.
        return
      }

      const newVersion = promptLigoUpdate(ligoPath, date)
      switch (typeof newVersion) {
        case 'string':
          validateSemver(version)
          break;
        case 'number':
          // TODO: Replace with actual LIGO release date with LIGO LSP.
          // Note: month is 0-indexed
          if (newVersion < Date.UTC(2023, 2 - 1, 14)) {
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
  await validateRollingRelease(data)
}
