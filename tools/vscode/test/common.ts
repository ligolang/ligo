import * as path from 'path'
import { execSync } from 'child_process';

export enum Dialect {
  CAMELIGO,
  JSLIGO
}

export function getExt(lang: Dialect): string {
  switch (lang) {
    case Dialect.CAMELIGO:
      return 'mligo'
    case Dialect.JSLIGO:
      return 'jsligo'
    default:
      throw new Error(`Unrecognized dialect: ${lang}`)
  }
}

export function getLang(lang: Dialect): string {
  switch (lang) {
    case Dialect.CAMELIGO:
      return 'CameLIGO'
    case Dialect.JSLIGO:
      return 'JsLIGO'
    default:
      throw new Error(`Unrecognized dialect: ${lang}`)
  }
}

// TIHI
export const contractsDir: string = process.env.CONTRACTS_DIR || path.join(__dirname, '..', '..', '..', 'test', 'contracts')

export function installLigoLibrary(path: string) {
  execSync("ligo install", { cwd: path, timeout: 50000 });
}

export function delay(ms: number) {
  return new Promise( resolve => setTimeout(resolve, ms) );
}
