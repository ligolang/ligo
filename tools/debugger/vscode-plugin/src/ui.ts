// SPDX-FileCopyrightText: 2020 Tocqueville Group
// SPDX-License-Identifier: LicenseRef-MIT-TQ
//
// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA
//
// SPDX-FileCopyrightText: 2022 Microsoft
// SPDX-License-Identifier: LicenseRef-MIT-Microsoft

// Useful UI elements which is used to make the plugin more pleasant to use.

import * as vscode from 'vscode';
import { QuickPickItem } from 'vscode';
import { Maybe, Ref, isDefined, InputBoxType, InputValueType, InputValidationResult, ContractMetadata } from './base'
import { LigoDebugContext, ValueAccess } from './LigoDebugContext'

const suggestTypeValue = (mitype: string): { value: string, selection?: [number, number] } => {
	const startsWith = (prefix: string, str: string): boolean =>
		str.substring(0, prefix.length) === prefix

	// Some of these assume that supplying Michelson values is possible.
	if (mitype === "string") {
		return { value: "\"\"", selection: [1, 1] }
	}
	else if (mitype === "int" || mitype === "nat" || mitype === "mutez") {
		return { value: "0" }
	}
	else if (mitype === "bytes") {
		// We select 0x, not put the cursor at the end, because the user will likely
		// copypaste a ready value from somewhere else.
		return { value: "0x" }
	}
	else if (mitype === "timestamp") {
		let defTime = new Date().toISOString()
		return { value: "\"" + defTime + "\"", selection: [1, defTime.length + 1] }
	}
	else if (startsWith("option", mitype)) {
		return { value: "michelson:None" }
	}
	else {
		return { value: "" }
	}
}

const oldValueSelection = (mitype: string, oldVal: string): Maybe<[number, number]> => {
	const startsWith = (prefix: string, str: string): boolean =>
		str.substring(0, prefix.length) === prefix
	const len = oldVal.length

	if (oldVal.charAt(0) == "\"" && (mitype === "string" || mitype === "timestamp" || mitype === "address")) {
		return [1, len - 1]
	}
	else if ((oldVal.charAt(0) == "{" || oldVal.charAt(0) == "[")
		&& (startsWith("list", mitype) || startsWith("set", mitype) || startsWith("map", mitype) || startsWith("big_map", mitype))) {
		return [1, len - 1]
	}
}

// Create QuickPick which remembers previously
// inputted value in workspace storage.
export async function createRememberingQuickPick (
		contractMetadata : ContractMetadata,
		placeHolder: string
	) : Promise<Maybe<string>> {

		const currentFilePath = vscode.window.activeTextEditor?.document.uri.fsPath
		const entrypoints = currentFilePath && contractMetadata.michelsonEntrypoints
		const pickerOptions: QuickPickItem[] =
			entrypoints ?
				Object.entries(entrypoints).map(([name, _michelsonType]) => {
					return {
						label: name,
						description: ""
					}
				}) :
				[]

		const quickpick =
			pickerOptions.length == 1
				? { then: (pass) => pass(pickerOptions[0]) }
				: vscode.window.showQuickPick<QuickPickItem>
					(pickerOptions,
						{
							placeHolder,
							ignoreFocusOut: true
						}
					)

		return quickpick.then(newVal => {
			if (newVal && newVal.label === "default") {
				newVal.label = "";
			}
			return newVal?.label;
		})
	}

export async function getEntrypoint (
		context: LigoDebugContext,
		validateEntrypoint: (entrypoint: string) => Promise<Maybe<string>>,
		entrypointsList: string[]
	) : Promise<Maybe<string>> {

		interface State {
			pickedEntrypoint: string;
		}

		async function askForEntrypoint(input: MultiStepInput<State>, state: Ref<Partial<State>>) {
			const currentFilePath = vscode.window.activeTextEditor?.document.uri.fsPath
			const entrypoints: QuickPickItem[] = currentFilePath && entrypointsList.map(label => ({ label }));

			const remembered = context.workspaceState.lastEntrypoint();

			if (entrypoints.length <= 1) {
				if (entrypoints.length === 0) {
					throw new Error("Given contract doesn't have any entrypoints");
				}
				state.ref.pickedEntrypoint = entrypoints[0].label;
				return;
			}

			let activeItem: Maybe<QuickPickItem>;
			if (isDefined(remembered.value)) {
				for (let entrypoint of entrypoints) {
					if (entrypoint.label === remembered.value) {
						activeItem = entrypoint;
						break;
					}
				}
			}

			const pick: QuickPickItem = await input.showQuickPick({
				totalSteps: 1,
				items: entrypoints,
				activeItem,
				placeholder: "Choose an entrypoint to run"
			});

			const validateResult = await validateEntrypoint(pick.label);
			if (validateResult) {
				vscode.window.showWarningMessage(validateResult);
				input.doNotRecordStep();
				return (input: MultiStepInput<State>, state: Ref<Partial<State>>) => askForEntrypoint(input, state);
			} else {
				remembered.value = pick.label;
				state.ref.pickedEntrypoint = pick.label;
			}
		}

		const result: State =
			await MultiStepInput.run(
				(input: MultiStepInput<State>, state: Ref<Partial<State>>) => askForEntrypoint(input, state)
			) as State;

		return result.pickedEntrypoint;
}

export async function getParameterOrStorage(
		context: LigoDebugContext,
		validateInput: (inputType: InputBoxType, valueType: InputValueType) => (value: string) => Promise<Maybe<string>>,
		inputBoxType: InputBoxType,
		placeHolder: string,
		prompt: string,
		ligoEntrypoint: string,
		contractMetadata: ContractMetadata,
		michelsonEntrypoint: Maybe<string>
	): Promise<Maybe<string>> {

	const totalSteps = 1;
	const rememberedFormat = context.workspaceState.lastParameterOrStorageFormat(inputBoxType, ligoEntrypoint)

	class SwitchButton implements vscode.QuickInputButton {
		public typ: InputValueType;
		public tooltip: string;

		constructor(public iconPath: vscode.Uri, typ: InputValueType) {
			this.typ = typ;
			this.tooltip = "Input in " + typ + " format";
		}

		static readonly LigoSwitch = new SwitchButton(
			vscode.Uri.file(context.asAbsolutePath('resources/ligo.png')),
			"LIGO"
		);

		static readonly MichelsonSwitch = new SwitchButton(
			vscode.Uri.file(context.asAbsolutePath('resources/tezos.png')),
			"Michelson"
		);
	}

	interface State {
		value: string;
		currentSwitch: SwitchButton;
	}

	async function askValue(input: MultiStepInput<State>, state: Ref<Partial<State>>) {
		var rememberedVal: ValueAccess<string>;

		let placeholderExtra: string = ''
		let michelsonType: string = ''
		switch (inputBoxType) {
			case "parameter":
				michelsonType = contractMetadata.parameterMichelsonType
				// Consider picked entrypoint in the key to remember value depending on an entrypoint
				rememberedVal = context.workspaceState.lastParameterOrStorageValue(inputBoxType, ligoEntrypoint, michelsonEntrypoint);
				if (michelsonEntrypoint) {
					placeholderExtra = " for '" + michelsonEntrypoint + "' entrypoint"
				}
				break

			case "storage":
				michelsonType = contractMetadata.storageMichelsonType
				rememberedVal = context.workspaceState.lastParameterOrStorageValue(inputBoxType, ligoEntrypoint);
				break;
		}

		const previousVal = rememberedVal.value
		const defaultValue =
			isDefined(previousVal)
				? { value: previousVal, selection: oldValueSelection(michelsonType, previousVal) }
				: suggestTypeValue(michelsonType)

		// Unfortunately, we can't use 'valueSelection' in low-level 'createInputBox'.
		// (https://github.com/microsoft/vscode/issues/56759)
		const pick = await input.showInputBox({
			totalSteps,
			placeholder: placeHolder + placeholderExtra,
			prompt,
			value: defaultValue.value,
			validate: validateInput(inputBoxType, state.ref.currentSwitch.typ),
			buttons: [state.ref.currentSwitch],
			// Keep input box open if focus is moved out
			ignoreFocusOut: true
		});

		if (isDefined(input.getCurrentValue())) {
			rememberedVal.value = input.getCurrentValue();
		}

		input.doNotRecordStep();

		if (pick instanceof SwitchButton) {
			switch (pick) {
				case SwitchButton.LigoSwitch:
					state.ref.currentSwitch = SwitchButton.MichelsonSwitch;
					break;
				case SwitchButton.MichelsonSwitch:
					state.ref.currentSwitch = SwitchButton.LigoSwitch;
					break;
			}

			rememberedFormat.value = state.ref.currentSwitch.typ;

			return (input: MultiStepInput<State>, state: Ref<Partial<State>>) => askValue(input, state);
		} else {
			state.ref.value = pick;
		}
	}

	let switchButton: SwitchButton;
	switch(rememberedFormat.value) {
		case "LIGO":
		case undefined:
			switchButton = SwitchButton.LigoSwitch;
			break;
		case "Michelson":
			switchButton = SwitchButton.MichelsonSwitch;
			break;
	}

	const result: State =
		await MultiStepInput.run(
			(input: MultiStepInput<State>, state) => askValue(input, state),
			{ currentSwitch: switchButton }
		) as State;

	if (isDefined(result.value) && isDefined(result.currentSwitch.typ)) {
		return result.value + '@' + result.currentSwitch.typ;
	}
}

class InputFlowAction {
	static back = new InputFlowAction();
	static cancel = new InputFlowAction();
}

type InputStep<S> = (input: MultiStepInput<S>, state: Ref<Partial<S>>) => Thenable<InputStep<S> | void>;

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

class MultiStepInput<S> {

	static async run<S>(start: InputStep<S>, state?: Partial<S>) {
		const input = new MultiStepInput<S>();
		if (state) {
			input.state.ref = state
		}
		return input.stepThrough(start);
	}

	private state: Ref<Partial<S>> = { ref: {} }
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

	private async stepThrough<T>(start: InputStep<S>) {
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

		return this.state.ref as S
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

					isObviouslyInvalid(text: string): boolean {
						return text.trim() === '';
					}

					onValidationResult(validationMessage: InputValidationResult): void {
						input.validationMessage = validationMessage;
					}
				}
				validationTrigger.fire(value);

				disposables.push(
					input.onDidTriggerButton(item => {
						if (item === vscode.QuickInputButtons.Back) {
							reject(InputFlowAction.back);
						} else {
							resolve(<any>item);
						}
					}),
					input.onDidAccept(async () => {
						validationTrigger.fire(input.value);

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
					input.onDidChangeValue(value => validationTrigger.fire(value)),
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
				validationTrigger.fire(value);
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
			} else {
				const oldStatus = this.status;
				this.status = { type: "busy" };
				try {
					const validationMessage = await this.validate(value);
					this.onValidationResult(validationMessage);
					this.status =
						{ type: "validated", value: value
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
			then(onFulfilled: (passingValue: Maybe<V>) => void, onRejected: (reason: any) => void): Thenable<void> {
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
