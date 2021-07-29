import { execFileSync } from 'child_process'

import * as axios from 'axios'
import * as semver from 'semver'
import * as vscode from 'vscode'

import { extensionId } from './common'

/* eslint-disable camelcase */
/**
 * Stripped version of the release type returned by GitLab with fields that are
 * interesting to us.
 */
type Release = {
  name: string
  tag_name: string
  released_at: Date
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

async function getLigoReleases(): Promise<Release[] | undefined> {
  // https://stackoverflow.com/a/53126068/10213577
  const ligoGitLabProjectId = 12294987
  const releasesUrl = `https://gitlab.com/api/v4/projects/${ligoGitLabProjectId}/releases/`
  return axios.default
    .get(releasesUrl)
    .then((res) => res.data)
    .catch((err) => vscode.window.showErrorMessage(`Could not fetch LIGO releases: ${err.message}`))
}

async function getLatestLigoRelease(): Promise<Release | undefined> {
  const releases = await getLigoReleases()
  if (!releases || releases.length === 0) {
    return undefined
  }

  return releases[0]
}

function getLigoPath(): string | undefined {
  const config = vscode.workspace.getConfiguration(extensionId)
  const ligoPath = config.get<string>('ligoBinaryPath')
  if (ligoPath) {
    return ligoPath
  }

  try {
    return execFileSync('which', ['ligo']).toString().trim()
  } catch {
    return undefined
  }
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
): Promise<void> {
  const latestRelease = await getLatestLigoRelease()
  if (!latestRelease) {
    return
  }

  switch (typeof installedVersionIdentifier) {
    // Semantic version
    case 'string':
      if (semver.gte(installedVersionIdentifier, latestRelease.tag_name)) {
        return
      }
      break
    // Date of some rolling release
    case 'number':
      if (new Date(installedVersionIdentifier) >= latestRelease.released_at) {
        return
      }
      break
    default:
      vscode.window.showErrorMessage(`Unknown version: ${installedVersionIdentifier}`)
  }

  const answer = await vscode.window.showInformationMessage(
    'A new LIGO version is available. Would you like to update?',
    'Update',
    'Cancel',
  )

  switch (answer) {
    case 'Update':
      openLigoReleases()
      break
    case 'Cancel':
    default:
      break
  }
}

export default async function updateLigo(): Promise<void> {
  const ligoPath = getLigoPath()

  let data: string
  try {
    if (!ligoPath) {
      throw new Error('Undefined path')
    }

    data = execFileSync(ligoPath, ['--version']).toString().trim()
  } catch (err) {
    const answer = await vscode.window.showInformationMessage(
      `Could not find a LIGO installation on your computer or the installation is invalid: ${err.message}`,
      'Choose path',
      'Download',
      'Cancel',
    )

    const config = vscode.workspace.getConfiguration(extensionId)
    switch (answer) {
      case 'Choose path': {
        const uris = await vscode.window.showOpenDialog({ canSelectMany: false })
        if (!uris || uris.length === 0) {
          return
        }

        config.update('ligoBinaryPath', uris[0].fsPath)
        updateLigo()
        return
      }
      case 'Download':
        openLigoReleases()
        return
      case 'Cancel':
      default:
        return
    }
  }

  const semverTest = semver.valid(semver.coerce(data))
  if (semverTest) {
    promptLigoUpdate(ligoPath, semverTest)
    return
  }

  const commitTest = /Rolling release\nCommit SHA: [0-9a-f]{40}\nCommit Date: (.+)/
  const commitDate = commitTest.exec(data)
  if (commitDate && commitDate.length === 2) {
    const date: number = Date.parse(commitDate[1])
    if (Number.isNaN(date)) {
      // Parse failed; not a date.
      return
    }

    promptLigoUpdate(ligoPath, date)
  } else {
    vscode.window.showErrorMessage(`Could not identity the installed LIGO version: ${data}`)
  }
}
