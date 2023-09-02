import * as vscode from 'vscode';

export abstract class LigoExtension {
  protected context: vscode.ExtensionContext;

  /* These methods should be called exactly once in extension.ts */
  public abstract activate(context: vscode.ExtensionContext): void;
  public abstract deactivate(): Thenable<void> | void | undefined;
}
