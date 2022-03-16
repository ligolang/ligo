import * as path from 'path'
import * as vscode from 'vscode'

export enum Dialect {
  PASCALIGO,
  CAMELIGO,
  REASONLIGO,
}

export function getExt(lang: Dialect): string {
  switch (lang) {
    case Dialect.PASCALIGO:
      return 'ligo'
    case Dialect.CAMELIGO:
      return 'mligo'
    case Dialect.REASONLIGO:
      return 'religo'
    default:
      throw new Error(`Unrecognized extesion: ${lang}`)
  }
}

export function getLang(lang: Dialect): string {
  switch (lang) {
    case Dialect.PASCALIGO:
      return 'PascaLIGO'
    case Dialect.CAMELIGO:
      return 'CameLIGO'
    case Dialect.REASONLIGO:
      return 'ReasonLIGO'
    default:
      throw new Error(`Unrecognized extesion: ${lang}`)
  }
}

export default async function activate(uri: vscode.Uri) {
  const ext = vscode.extensions.getExtension('ligolang-publish.ligo-vscode')
  await ext.activate()
  try {
    const doc = await vscode.workspace.openTextDocument(uri)
    await vscode.window.showTextDocument(doc)
  } catch (e) {
    console.error(e)
  }
}

// TIHI
export const contractsDir: string = process.env.CONTRACTS_DIR || path.join(__dirname, '..', '..', '..', '..', 'squirrel', 'test', 'contracts')
