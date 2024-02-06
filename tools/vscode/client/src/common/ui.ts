// SPDX-FileCopyrightText: 2020 Tocqueville Group
// SPDX-License-Identifier: LicenseRef-MIT-TQ
//
// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA
//
// SPDX-FileCopyrightText: 2022 Microsoft
// SPDX-License-Identifier: LicenseRef-MIT-Microsoft

import { QuickPickItem } from "vscode";
import * as vscode from 'vscode'
import { InputValidationResult, isDefined, Maybe, Ref } from "./base";

class InputFlowAction {
  static back = new InputFlowAction();
  static cancel = new InputFlowAction();
}

type InputStep<S> = (input: MultiStepInput<S>, state: Ref<S>) => Thenable<InputStep<S> | void>;

interface QuickPickParameters<T extends QuickPickItem> {
  totalSteps: number;
  items: T[];
  activeItem?: T;
  buttons?: vscode.QuickInputButton[];
  placeholder: string;
}

interface InputBoxParameters {
  placeholder: string;
  totalSteps: number;
  value: string;
  prompt: string;
  validate: (value: string) => Promise<InputValidationResult>;
  buttons?: vscode.QuickInputButton[];
  ignoreFocusOut: boolean;
}

export class MultiStepInput<S> {
  static async run<S>(start: InputStep<S>, state: S) {
    const input = new MultiStepInput<S>();
    input.state = { ref: state };

    return input.stepThrough(start);
  }

  private state: Ref<S>;
  private current?: vscode.QuickInput;
  private steps: InputStep<S>[] = [];
  private shouldRecordStep: boolean = true;

  public doNotRecordStep() {
    this.shouldRecordStep = false;
  }

  public getCurrentValue(): Maybe<string> {
    // Since 'InputBox' is interface we can't check 'this.current' type
    // with 'instanceof'.
    if (this.current && 'value' in this.current) {
      return (this.current as vscode.InputBox).value;
    } else {
      return undefined;
    }
  }

  private async stepThrough(start: InputStep<S>) {
    let step: InputStep<S> | void = start;
    while (step) {
      this.steps.push(step);
      if (this.current) {
        this.current.enabled = false;
        this.current.busy = true;
      }
      try {
        step = await step(this, this.state);
      } catch (err) {
        switch (err) {
          case InputFlowAction.back:
            this.steps.pop();
            step = this.steps.pop();
            break;
          case InputFlowAction.cancel:
            step = undefined;
            break;
          default:
            throw err;
        }
      } finally {
        if (!this.shouldRecordStep) {
          this.steps.pop();
          this.shouldRecordStep = true;
        }
      }
    }
    if (this.current) {
      this.current.dispose();
    }

    return this.state.ref;
  }

  async showQuickPick<T extends QuickPickItem, P extends QuickPickParameters<T>>({ totalSteps, items, activeItem, buttons, placeholder }: P) {
    const disposables: vscode.Disposable[] = [];
    try {
      return await new Promise<T | (P extends { buttons: (infer I)[] } ? I : never)>((resolve, reject) => {
        const input = vscode.window.createQuickPick<T>();
        if (totalSteps > 1) {
          input.step = this.steps.length;
          input.totalSteps = totalSteps;
        }
        input.placeholder = placeholder;
        input.items = items;
        if (activeItem) {
          input.activeItems = [activeItem];
        }
        input.buttons = [
          ...(this.steps.length > 1 ? [vscode.QuickInputButtons.Back] : []),
          ...(buttons || [])
        ];
        disposables.push(
          input.onDidTriggerButton(item => {
            if (item === vscode.QuickInputButtons.Back) {
              reject(InputFlowAction.back);
            } else {
              resolve(<any>item);
            }
          }),
          input.onDidChangeSelection(items => resolve(items[0])),
          input.onDidHide(() => {
            (async () => {
              reject(InputFlowAction.cancel);
            })()
              .catch(reject);
          })
        );
        if (this.current) {
          this.current.dispose();
        }
        this.current = input;
        this.current.show();
      });
    } finally {
      disposables.forEach(d => d.dispose());
    }
  }

  async showInputBox<P extends InputBoxParameters>({ placeholder, totalSteps, value, prompt, validate, buttons, ignoreFocusOut }: P) {
    const disposables: vscode.Disposable[] = [];
    try {
      return await new Promise<string | (P extends { buttons: (infer I)[] } ? I : never)>((resolve, reject) => {
        const input = vscode.window.createInputBox();
        if (totalSteps > 1) {
          input.step = this.steps.length;
          input.totalSteps = totalSteps;
        }
        input.placeholder = placeholder;
        input.value = value || '';
        input.prompt = prompt;
        input.buttons = [
          ...(this.steps.length > 1 ? [vscode.QuickInputButtons.Back] : []),
          ...(buttons || [])
        ];
        input.ignoreFocusOut = ignoreFocusOut;

        const validationTrigger = new class extends ValidationTrigger<string>{
          validate = validate

          override isObviouslyInvalid(text: string): boolean {
            return text === '';
          }

          onValidationResult(validationMessage: InputValidationResult): void {
            input.validationMessage = validationMessage;
          }
        }
        const submitForValidation =
          (value: string) => validationTrigger.fire(value.trim());

        disposables.push(
          input.onDidTriggerButton(item => {
            if (item === vscode.QuickInputButtons.Back) {
              reject(InputFlowAction.back);
            } else {
              resolve(<any>item);
            }
          }),
          input.onDidAccept(async () => {
            submitForValidation(input.value);

            // Note: turns out, "enabled" flag does not really work
            // and so the user's input is not blocked, see
            // https://github.com/microsoft/vscode/issues/159906
            //
            // This means that the user can type something after
            // hitting Enter and that will be accepted, however only
            // if it passes validation.
            //
            // Let's treat it as a feature rather than a bug?
            input.enabled = false;
            input.busy = true;

            const passingValue = await validationTrigger.stablePassingValue()
            if (isDefined(passingValue)) {
              resolve(passingValue)
            }

            input.enabled = true;
            input.busy = false;
          }),
          input.onDidChangeValue(submitForValidation),
          input.onDidHide(() => {
            (async () => {
              reject(InputFlowAction.cancel);
            })()
              .catch(reject);
          }),
          validationTrigger
        );
        if (this.current) {
          this.current.dispose();
        }
        this.current = input;
        submitForValidation(value);
        this.current.show();
      });
    } finally {
      disposables.forEach(d => d.dispose());
    }
  }
}


// This class provides functionality close to EventEmitter but specialized to
// events validation.
//
// Validation is a user-defined asynchronous and potentially long action,
// and this class helps to work with such validation in a safe manner,
// avoiding bugs due to race conditions.
//
// This is implemented via sequentially processing the values, keeping a queue
// of values that are pending for validation, however this queue has capacity 1
// and older values are dropped.
abstract class ValidationTrigger<V> implements vscode.Disposable {
  // An inner events emitter that helps to keep `fire` of non-Promise type.
  private emitter: vscode.EventEmitter<V> = new vscode.EventEmitter();

  // What are we doing at the moment / what we just did.
  private status:
    | { type: "neverCalled" }
    | { type: "busy" }
    | { type: "validated", value: V, successfull: boolean }
    = { type: "neverCalled" }

  // The last value that has been submitted for validation, given that
  // we are already busy validating something.
  private pendingValue: Maybe<V>

  // Callbacks added by `awaitResult` that are yet waiting.
  private resultWaiters: ((passingValue: Maybe<V>) => void)[] = new Array();

  public constructor() {
    this.emitter.event(v => this.executeValidation(v));
  }

  // Submit a value for validation.
  public fire(value: V): void {
    this.emitter.fire(value)
  }

  // Validation function.
  protected abstract validate(value: V): Promise<InputValidationResult>

  // Values that are obviously invalid and we don't want to show a "bad value"
  // error for them.
  protected isObviouslyInvalid(value: V): boolean { return false; }

  // Invoked when some validation function is completed.
  //
  // This will be run in the same order in which values were passed for
  // validation.
  //
  // This will be run as many times as values were submitted for valitation via
  // 'fire' method, however subsequent submission of the same value will be ignored.
  protected abstract onValidationResult(validationResult: InputValidationResult): void

  // Safely run validation for a new value.
  private async executeValidation(value: V): Promise<void> {
    try {
      // Check if we are busy validating something.
      if (this.status.type == "busy") {
        this.pendingValue = value;

        // We exit, relying on the function that runs `this.executeValidation`
        // to also run validation for the new value later.
        return;
      }

      // The validation queue is empty, we can start the validation of
      // the new value.

      if (this.isObviouslyInvalid(value)) {
        // Report that validation succeeded, making any old validation
        // error disappear
        this.onValidationResult(undefined);
        this.status = { type: "validated", value: value, successfull: false };
      } else if (this.status.type == 'validated' && this.status.value === value) {
        // Repeated call, do nothing
      } else {
        const oldStatus = this.status;
        this.status = { type: "busy" };
        try {
          const validationMessage = await this.validate(value);
          this.onValidationResult(validationMessage);
          this.status =
          {
            type: "validated", value: value
            , successfull: !isDefined(validationMessage)
          };
        } catch (err) {
          this.status = oldStatus;
          throw err;
        }
      }

      await this.keepValidatingPending(this.status.value, this.status.successfull);
    } catch (err) {
      vscode.window.showWarningMessage("Internal error in validation: " + err)
    }

  }

  // Keep processing the values that are pending for validation.
  //
  // This function may execute for an arbitrary amount of time, so it must
  // appear only as the last action in the sequence or in a separate thread.
  private async keepValidatingPending(lastValidatedValue: V, lastValidationSuccessful: boolean): Promise<void> {
    if (isDefined(this.pendingValue)) {
      // Continue with validating a new value.
      const pending = this.pendingValue
      this.pendingValue = undefined;
      this.executeValidation(pending);
    } else {
      // Pending values queue is empty.

      // Call result waiters.
      const passingValue = lastValidationSuccessful ? lastValidatedValue : undefined;
      this.resultWaiters.forEach(waiter => waiter(passingValue));
      this.resultWaiters = new Array();
    }

  }

  // Awaits for a moment when we have validated all the values and no new values
  // yet come, and returns the last value if it passed validation, and
  // `undefined` otherwise.
  //
  // This rejects if no value has ever been validated.
  public stablePassingValue(): Thenable<Maybe<V>> {
    const this0 = this;
    return {
      // This is not entirely correct definition, it should be generic in
      // the type of returned `Thenable`; but for our cases this is enough.
      then(onFulfilled: (passingValue: Maybe<V>) => void, onRejected: (reason: any) => void): Thenable<Maybe<V>> {
        switch (this0.status.type) {
          case "neverCalled":
            onRejected("Validation has never been called");
            break;
          case "validated":
            const passingValue = this0.status.successfull ? this0.status.value : undefined
            onFulfilled(passingValue);
            break;
          case "busy":
            this0.resultWaiters.push(onFulfilled);
            break;
        }

        return this;
      }
    }
  }

  public dispose() {
    this.emitter.dispose();
  }
}
