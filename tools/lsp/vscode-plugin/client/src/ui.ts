import * as vscode from 'vscode'

let prevInput;

export default function createRememberingInputBox(
  title : string,
  placeHolder : string,
  defaultValue : string,
): Thenable<string | undefined> {
  return vscode.window.showInputBox({
    title,
    placeHolder,
    value: prevInput || defaultValue,
    ignoreFocusOut: true,
  }).then((newVal) => {
    prevInput = newVal;
    return newVal
  });
}
