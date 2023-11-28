// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA

// Utilities for managing context.

import * as vscode from 'vscode';
import { InputBoxType, InputValueLang, isDefined, Maybe } from './base';

// Our wrapper over ExtensionContext that provides custom set of operations.
export class LigoContext {
  context: vscode.ExtensionContext
  workspaceState: LigoLocalStorage
  globalState: LigoGlobalStorage

  constructor(context: vscode.ExtensionContext) {
    this.context = context
    this.workspaceState = new LigoLocalStorage(context.workspaceState)
    this.globalState = new LigoGlobalStorage(context.globalState)
  }

  asAbsolutePath(relativePath: string): string {
    return this.context.asAbsolutePath(relativePath)
  }

}

// A value with getter and setter referring to a cell in a storage
// (Memento class) associated with a particular key.
export interface ValueAccess<T> {
  get value(): Maybe<T>;
  set value(val: Maybe<T>);
}

class MementoCell<T> implements ValueAccess<T> {
  state: vscode.Memento
  field: string

  constructor(state: vscode.Memento, field: string) {
    this.state = state
    this.field = field
  }

  get value(): Maybe<T> {
    return this.state.get(this.field)
  }

  set value(val: Maybe<T>) {
    this.state.update(this.field, val)
  }
}

// Never remembers any object.
class NoValueCell<T> implements ValueAccess<T> {
  get value(): Maybe<T> { return undefined }
  set value(_newVal: Maybe<T>) { }
}

abstract class AbstractLigoStorage {
  public state: vscode.Memento;

  constructor(state: vscode.Memento) {
    this.state = state;
  }

  // Accepts a set of keys that point to a cell in storage and returns
  // the access object to that cell.
  //
  // 'undefined' keys will be skipped when constructing the total key.
  protected access(...keys: Maybe<string>[]): ValueAccess<any> {
    const keysClear = keys.filter(k => k !== undefined)

    const currentFilePath = vscode.window.activeTextEditor?.document.uri.fsPath

    if (isDefined(currentFilePath)) {
      keysClear.push(currentFilePath)
      return new MementoCell(this.state, keysClear.join("_"))
    } else {
      return new NoValueCell()
    }
  }
}

// Local storage with our custom format.
export class LigoLocalStorage extends AbstractLigoStorage {
  constructor(state: vscode.Memento) {
    super(state);
  }

  lastModuleName(): ValueAccess<string> {
    return this.access("quickpick", "module", "name");
  }

  lastConfigPath(): ValueAccess<string> {
    return this.access("inputbox", "config", "path");
  }

  lastParameterOrStorageValue(type: InputBoxType, moduleName: string, entrypoint: string)
    : ValueAccess<[string, InputValueLang]> {
    return this.access("quickpick", "switch", "button", type, moduleName, entrypoint)
  }

  askedForLigoConfig(): ValueAccess<boolean> {
    return this.access("ask", "for", "ligo", "config");
  }

  lspRememberedValue(...keys: string[]): ValueAccess<string> {
    return this.access("lsp", ...keys);
  }
}

export class LigoGlobalStorage extends AbstractLigoStorage {
  constructor(state: vscode.Memento) {
    super(state);
  }

  public askOnStartCommandChanged(): ValueAccess<boolean> {
    return this.access("command", "askOnStart");
  }
}
