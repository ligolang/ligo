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
      throw new Error(`Unrecognized dialect: ${lang}`)
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
      throw new Error(`Unrecognized dialect: ${lang}`)
  }
}

// TIHI
export const contractsDir: string = process.env.CONTRACTS_DIR || path.join(__dirname, '..', '..', '..', '..', 'squirrel', 'test', 'contracts')
