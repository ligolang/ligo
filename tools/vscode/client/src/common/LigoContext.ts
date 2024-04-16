// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA

// Utilities for managing context.

import * as vscode from 'vscode';
import { InputBoxType, InputValueLang, isDefined, Maybe } from './base';

/**
 * A wrapper over {@link vscode.ExtensionContext | ExtensionContext} that
 * provides a custom set of operations.
 */
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

/**
 * A value with getter and setter referring to a cell in a storage
 * ({@link vscode.Memento | Memento} class) associated with a particular key.
 */
export interface ValueAccess<T> {
  get value(): Maybe<T>;
  set value(val: Maybe<T>);
}

/** A value access cell remembering a string input. */
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

/** A value access cell that never remembers any object. */
class NoValueCell<T> implements ValueAccess<T> {
  get value(): Maybe<T> { return undefined }
  set value(_newVal: Maybe<T>) { }
}

/**
 * Represents an abstract storage mechanism. Provides methods for accessing and
 * managing stored data.
 */
abstract class AbstractLigoStorage {
  /**
   * The state object for storing data using VSCode's
   * {@link vscode.Memento | Memento}.
   */
  public state: vscode.Memento;

  constructor(state: vscode.Memento) {
    this.state = state;
  }

  /**
   * Accepts a set of keys that point to a cell in storage and returns the
   * access object to that cell.
   *
   * `undefined` keys will be skipped when constructing the total key.
   */
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

/**
 * Represents a local storage with a custom format specific to LIGO storages.
 */
export class LigoLocalStorage extends AbstractLigoStorage {
  constructor(state: vscode.Memento) {
    super(state);
  }

  /**
   * Returns the {@link ValueAccess} for accessing the last module name in the
   * local storage.
   */
  lastModuleName(): ValueAccess<string> {
    return this.access("quickpick", "module", "name");
  }

  /**
   * Returns the {@link ValueAccess} for accessing the last configuration path
   * in the local storage.
   */
  lastConfigPath(): ValueAccess<string> {
    return this.access("inputbox", "config", "path");
  }

  /**
    * Returns the {@link ValueAccess} object for accessing the last parameter or
    * storage value stored in local storage.
    * @param type The type of input box (parameter or storage).
    * @param moduleName The name of the module with entry-points.
    * @param entrypoint The name of the entry point.
    */
  lastParameterOrStorageValue(type: InputBoxType, moduleName: string, entrypoint: string)
    : ValueAccess<[string, InputValueLang]> {
    return this.access("quickpick", "switch", "button", type, moduleName, entrypoint)
  }

  /**
   * Returns the {@link ValueAccess} for accessing whether the user has been
   * asked for the LIGO configuration.
   */
  askedForLigoConfig(): ValueAccess<boolean> {
    return this.access("ask", "for", "ligo", "config");
  }

  /**
   * Returns the {@link ValueAccess} for accessing a value remembered by a LIGO
   * LSP input box.
   */
  lspRememberedValue(...keys: string[]): ValueAccess<string> {
    return this.access("lsp", ...keys);
  }
}

/** Represents a global storage for `ligo-vscode`. */
export class LigoGlobalStorage extends AbstractLigoStorage {
  constructor(state: vscode.Memento) {
    super(state);
  }

  /**
   * Returns the {@link ValueAccess} for accessing the state of the
   * `"askOnStart"` command.
   */
  public askOnStartCommandChanged(): ValueAccess<boolean> {
    return this.access("command", "askOnStart");
  }
}
