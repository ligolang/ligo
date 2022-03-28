import * as vscode from 'vscode'
import { execFileSync } from 'child_process'
import { extname } from 'path';
import { createRememberingInputBox, createQuickPickBox } from './ui'
import { getLigoPath } from './updateLigo'

const ligoOutput = vscode.window.createOutputChannel('LIGO Compiler')
let lastContractPath;

function extToDialect(ext : string) {
  switch (ext) {
    case '.ligo': return 'pascaligo'
    case '.mligo': return 'cameligo'
    case '.religo': return 'reasonligo'
    default:
      console.error('Unknown dialect');
      return undefined
  }
}

async function executeLigoCommand(command, getSyntax = false, showOutput = true) {
  let path = vscode.window.activeTextEditor.document.uri.fsPath;
  const ext = extname(path);
  console.log(path)
  if (ext !== '.ligo' && ext !== '.mligo' && ext !== '.religo') {
    if (!lastContractPath) {
      return undefined;
    }
    path = lastContractPath;
  }

  const ligoPath = getLigoPath(vscode.workspace.getConfiguration());
  lastContractPath = path;

  if (!ligoPath || ligoPath === '') {
    vscode.window.showWarningMessage('LIGO executable not found. Aborting ...');
    return undefined;
  }

  const finalCommand = !getSyntax ? command(path) : command(path, extToDialect(extname(path)));
  try {
    const result = execFileSync(ligoPath, finalCommand)
    if (showOutput) {
      ligoOutput.appendLine(result)
    }
    return result;
  } catch (error) {
    ligoOutput.appendLine(error.message);
  } finally {
    ligoOutput.show()
  }
  return undefined
}

export async function executeCompileContract() {
  const maybeEntrypoint = await createRememberingInputBox({
    title: 'Entrypoint',
    placeHolder: 'Enter entrypoint to compile',
    rememberingKey: 'compile-contract',
    defaultValue: 'main',
  });
  if (!maybeEntrypoint) {
    return;
  }
  executeLigoCommand((path: string) => ['compile', 'contract', path, '-e', maybeEntrypoint]);
}

export async function executeCompileExpression() {
  const listOfExpressions = await executeLigoCommand((path : string) => ['info', 'list-declarations', path], false, false)
  const exp = listOfExpressions.toString().split(':')[1].replace(/\s+/g, ' ').split(' ').slice(1, -1);
  const maybeExpression = await createQuickPickBox(exp, 'Expressions', 'Possible expressions for this contract')

  if (!maybeExpression) {
    return;
  }
  executeLigoCommand((path: string, syntax: string) => ['compile', 'expression', syntax, maybeExpression, '--init-file', path], true);
}

export async function executeDryRun() {
  const maybeParameter = await createRememberingInputBox({
    title: 'Parameter',
    placeHolder: 'Entrypoint parameter',
    rememberingKey: 'dry-run-parameter',
    defaultValue: undefined,
  })
  const maybeStorage = await createRememberingInputBox({
    title: 'Storage',
    placeHolder: 'Entrypoint storage',
    rememberingKey: 'dry-run-storage',
    defaultValue: undefined,
  })
  const maybeEntrypoint = await createRememberingInputBox({
    title: 'Entrypoint',
    placeHolder: 'Enter entrypoint to compile',
    rememberingKey: 'dry-run-entrypoint',
    defaultValue: 'main',
  });
  if (!maybeParameter || !maybeStorage || !maybeEntrypoint) {
    return;
  }
  executeLigoCommand((path: string) => ['run', 'dry-run', path, maybeParameter, maybeStorage, '-e', maybeEntrypoint]);
}

export async function executeEvaluateFunction() {
  const maybeEntrypoint = await createRememberingInputBox({
    title: 'Entrypoint',
    placeHolder: 'Enter function to compile',
    rememberingKey: 'call-entrypoint',
    defaultValue: 'main',
  });
  const maybeExpr = await createRememberingInputBox({
    title: 'Arguments',
    placeHolder: 'Function arguments',
    rememberingKey: 'call-arg',
    defaultValue: undefined,
  })
  if (!maybeExpr || !maybeEntrypoint) {
    return;
  }
  executeLigoCommand((path: string) => ['run', 'evaluate-call', path, maybeExpr, '-e', maybeEntrypoint]);
}

export async function executeEvaluateValue() {
  const maybeEntrypoint = await createRememberingInputBox({
    title: 'Entrypoint',
    placeHolder: 'Enter an value or a function to compile',
    rememberingKey: 'call-value',
    defaultValue: '',
  });
  if (!maybeEntrypoint) {
    return;
  }
  executeLigoCommand((path: string) => ['run', 'evaluate-expr', path, '-e', maybeEntrypoint]);
}
