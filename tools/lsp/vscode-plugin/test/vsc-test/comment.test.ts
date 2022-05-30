import * as assert from 'assert'
import * as path from 'path'
import * as vscode from 'vscode'

import {
  contractsDir, Dialect, getExt, getLang,
} from '../common'

function getBlockComment(lang: Dialect): [string, string] {
  switch (lang) {
    case Dialect.PASCALIGO:
    case Dialect.CAMELIGO:
      return ['(*', '*)']
    case Dialect.JSLIGO:
    case Dialect.REASONLIGO:
      return ['/*', '*/']
    default:
      throw new Error(`Unrecognized dialect: ${lang}`)
  }
}

function lineCommentTest(lang: Dialect) {
  test(`Line comments are inserted correctly (${getLang(lang)})`, async () => {
    const ext = getExt(lang)
    const uri = vscode.Uri.file(path.join(contractsDir, 'bugs', `LIGO-425.${ext}`))
    const doc = await vscode.workspace.openTextDocument(uri)
    await vscode.window.showTextDocument(doc)
    const originalContents = doc.getText()

    await vscode.commands.executeCommand('editor.action.commentLine')
    const newContents = doc.getText()
    assert.equal(newContents, `// ${originalContents}`)

    await vscode.commands.executeCommand('editor.action.commentLine')
    const revertedContents = doc.getText()
    assert.equal(originalContents, revertedContents)
  }).timeout(5000) // 2000 is not enough
}

function blockCommentTest(lang: Dialect) {
  test(`Block comments are inserted correctly (${getLang(lang)})`, async () => {
    const ext = getExt(lang)
    const uri = vscode.Uri.file(path.join(contractsDir, 'bugs', `LIGO-425.${ext}`))
    const doc = await vscode.workspace.openTextDocument(uri)
    await vscode.window.showTextDocument(doc)
    const originalContents = doc.getText()

    // Need to select the line before adding block comments
    await vscode.commands.executeCommand('expandLineSelection')
    await vscode.commands.executeCommand('editor.action.blockComment')
    const newContents = doc.getText()
    const [left, right] = getBlockComment(lang)
    assert.equal(newContents, `${left} ${originalContents} ${right}`)

    await vscode.commands.executeCommand('expandLineSelection')
    await vscode.commands.executeCommand('editor.action.blockComment')
    const revertedContents = doc.getText()
    assert.equal(originalContents, revertedContents)
  }).timeout(5000) // 2000 is not enough
}

suite('LIGO: Insert comments (LIGO-425 regression)', () => {
  lineCommentTest(Dialect.PASCALIGO)
  lineCommentTest(Dialect.CAMELIGO)
  lineCommentTest(Dialect.REASONLIGO)
  lineCommentTest(Dialect.JSLIGO)
  blockCommentTest(Dialect.PASCALIGO)
  blockCommentTest(Dialect.CAMELIGO)
  blockCommentTest(Dialect.REASONLIGO)
  blockCommentTest(Dialect.JSLIGO)
})
