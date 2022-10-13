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
import { DebuggedContractSession, Maybe, Ref, isDefined, InputBoxType, InputValueType } from './base'
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
export const createRememberingQuickPick =
	(debuggedContract: Ref<DebuggedContractSession>,
		placeHolder: string
	) => async (config: any): Promise<Maybe<string>> => {

		if (!isDefined(debuggedContract.ref.contractMetadata)) {
			throw new Error("Internal error: metadata is not defined at the moment of user input")
		}

		const currentFilePath = vscode.window.activeTextEditor?.document.uri.fsPath
		const entrypoints = currentFilePath && debuggedContract.ref.contractMetadata.michelsonEntrypoints
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
			// Recreate an object to make it immune to changes of 'newVal'
			if (newVal) {
				debuggedContract.ref.pickedMichelsonEntrypoint = newVal.label
			} else {
				debuggedContract.ref.pickedMichelsonEntrypoint = undefined
			}

			if (newVal && newVal.label === "default") {
				newVal.label = ""
			}
			return newVal?.label
		})
	}

export const getEntrypoint = (
		context: LigoDebugContext,
		validateEntrypoint: (entrypoint: string) => Promise<Maybe<string>>,
		getContractMetadata: (entrypoint: string) => Promise<void>,
		debuggedContract: Ref<DebuggedContractSession>
	) => async (_config: any): Promise<Maybe<String>> => {

		interface State {
			pickedEntrypoint: string;
		}

		async function askForEntrypoint(input: MultiStepInput<State>, state: Ref<Partial<State>>) {
			if (!isDefined(debuggedContract.ref.entrypoints)) {
				throw new Error("Internal error: entrypoints must be defined");
			}

			const entrypoints: QuickPickItem[] = debuggedContract.ref.entrypoints.map(label => ({ label }));

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

		if (isDefined(result.pickedEntrypoint)) {
			await getContractMetadata(result.pickedEntrypoint);
			debuggedContract.ref.pickedLigoEntrypoint = result.pickedEntrypoint;
			return result.pickedEntrypoint;
		}
}

export const getParameterOrStorage = (
		context: LigoDebugContext,
		validateInput: (boxType: InputBoxType, valueType: InputValueType) => (value: string) => Promise<Maybe<string>>,
		inputBoxType: InputBoxType,
		placeHolder: string,
		prompt: string,
		debuggedContract: Ref<DebuggedContractSession>
	) => async (_config: any): Promise<Maybe<string>> => {

	if (!isDefined(debuggedContract.ref.pickedLigoEntrypoint)) {
		throw new Error("Internal error: LIGO entrypoint is not defined");
	}

	const ligoEntrypoint: string = debuggedContract.ref.pickedLigoEntrypoint;

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
		if (!isDefined(debuggedContract.ref.contractMetadata)) {
			throw new Error("Internal error: metadata is not defined at the moment of user input")
		}
		if (!isDefined(state.ref.currentSwitch)) {
			throw new Error("Internal error: debugging state is not initialized")
		}

		var rememberedVal: ValueAccess<string>;

		let placeholderExtra: string = ''
		let michelsonType: string = ''
		switch (inputBoxType) {
			case "parameter":
				michelsonType = debuggedContract.ref.contractMetadata.parameterMichelsonType
				// Consider picked entrypoint in the key to remember value depending on an entrypoint
				const entrypoint = debuggedContract.ref.pickedMichelsonEntrypoint
				rememberedVal = context.workspaceState.lastParameterOrStorageValue(inputBoxType, ligoEntrypoint, entrypoint);
				if (entrypoint) {
					placeholderExtra = " for '" + entrypoint + "' entrypoint"
				}
				break

			case "storage":
				michelsonType = debuggedContract.ref.contractMetadata.storageMichelsonType
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
	validate: (value: string) => Promise<string | undefined>;
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

					isObviouslyInvalid(text): boolean {
						return text.trim() === '';
					}

					onValidationResult(validationMessage: string | undefined): void {
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
						const value = input.value;
						input.enabled = false;
						input.busy = true;

						if (await validationTrigger.checkResult(value)) {
							resolve(value)
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
// Validation is a user-defined asynchronous action, and this class helps
// to work with such validation in a safe manner, avoiding bugs due to race
// conditions.
abstract class ValidationTrigger<V> implements vscode.Disposable {
	// An inner events emitter that helps to keep `fire` of non-Promise type.
	private emitter: vscode.EventEmitter<V> = new vscode.EventEmitter();

	// The last run validating function.
	validating: Promise<string | undefined>

	// The thing we last validated and the validation outcome.
	lastValidationResult: Maybe<{ value: V, result: boolean }>

	constructor() {
		this.emitter.event(v => this.executeValidation(v));
	}

	// Submit a value for validation.
	fire(value: V): void {
		this.emitter.fire(value)
	}

	// Validation function.
	abstract validate(value: V): Promise<string | undefined>

	// Values that are obviously invalid and we don't want to show a "bad value"
	// error for them.
	isObviouslyInvalid(value: V): boolean { return false; }

	// Invoked when some validation function is completed.
	//
	// This will be run in the same order in which values were passed for
	// validation. TODO: this property actually holds only if validation
	// function is sequencial and processes requests in FIFO
	// (our backend works exactly this way), this does not seem good.
	abstract onValidationResult(validationMessage: string | undefined): void

	// Safely run validation for a new value.
	private async executeValidation(value: V): Promise<void> {
		try {
			const current = this.validate(value);
			this.validating = current;
			const validationMessage = await current;

			// During validation a new value could come in - in this case the current
			// value can be skipped.
			if (current == this.validating) {
				if (this.isObviouslyInvalid(value)) {
					this.onValidationResult(undefined);
					return;
				}

				this.lastValidationResult = { value: value, result: !validationMessage };
				this.onValidationResult(validationMessage);
			}
		} catch (err) {
			vscode.window.showWarningMessage("Internal error in validation: " + err)
		}

	}

	// Checks that the given value is the one that has been validated last
	// and that it passed validation.
	//
	// This must be called when no new value can come in.
	async checkResult(value: V): Promise<boolean> {
		if (isDefined(this.lastValidationResult)) {
			return this.lastValidationResult.value === value && this.lastValidationResult.result
		} else {
			const result: boolean = !(await this.validate(value));
			this.lastValidationResult = { value, result };
			return result;
		}
	}

	dispose() {
		this.emitter.dispose();
	}
}
