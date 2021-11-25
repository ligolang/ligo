import * as path from 'path'
import * as vscode from 'vscode'

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
