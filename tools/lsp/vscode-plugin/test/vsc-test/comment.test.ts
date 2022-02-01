import * as assert from 'assert'
import * as path from 'path'
import * as vscode from 'vscode'

import activate, {
  contractsDir, Dialect, getExt, getLang,
} from '../common'

function getBlockComment(lang: Dialect): [string, string] {
  switch (lang) {
    case Dialect.PASCALIGO:
    case Dialect.CAMELIGO:
      return ['(*', '*)']
    case Dialect.REASONLIGO:
      return ['/*', '*/']
    default:
      throw new Error(`Unrecognized extesion: ${lang}`)
  }
}

async function lineCommentTest(lang: Dialect) {
  test(`Line comments are inserted correctly (${getLang(lang)})`, async () => {
    const ext = getExt(lang)
    const uri = await vscode.Uri.file(path.join(contractsDir, 'bugs', `LIGO-425.${ext}`))
    await activate(uri)
    const originalContents = vscode.window.activeTextEditor.document.getText()

    await vscode.commands.executeCommand('editor.action.commentLine')
    const newContents = vscode.window.activeTextEditor.document.getText()
    assert.equal(newContents, `// ${originalContents}`)

    await vscode.commands.executeCommand('editor.action.commentLine')
    const revertedContents = vscode.window.activeTextEditor.document.getText()
    assert.equal(originalContents, revertedContents)
  }).timeout(5000) // 2000 is not enough
}

async function blockCommentTest(lang: Dialect) {
  test(`Block comments are inserted correctly (${getLang(lang)})`, async () => {
    const ext = getExt(lang)
    const uri = await vscode.Uri.file(path.join(contractsDir, 'bugs', `LIGO-425.${ext}`))
    await activate(uri)
    const originalContents = vscode.window.activeTextEditor.document.getText()

    // Need to select the line before adding block comments
    await vscode.commands.executeCommand('expandLineSelection')
    await vscode.commands.executeCommand('editor.action.blockComment')
    const newContents = vscode.window.activeTextEditor.document.getText()
    const [left, right] = getBlockComment(lang)
    assert.equal(newContents, `${left} ${originalContents} ${right}`)

    await vscode.commands.executeCommand('expandLineSelection')
    await vscode.commands.executeCommand('editor.action.blockComment')
    const revertedContents = vscode.window.activeTextEditor.document.getText()
    assert.equal(originalContents, revertedContents)
  }).timeout(5000) // 2000 is not enough
}

suite('LIGO: Insert comments (LIGO-425 regression)', () => {
  lineCommentTest(Dialect.PASCALIGO)
  lineCommentTest(Dialect.CAMELIGO)
  lineCommentTest(Dialect.REASONLIGO)
  blockCommentTest(Dialect.PASCALIGO)
  blockCommentTest(Dialect.CAMELIGO)
  blockCommentTest(Dialect.REASONLIGO)
})
