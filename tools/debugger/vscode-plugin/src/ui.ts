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
import { ExtensionContext, QuickPickItem } from 'vscode';
import { DebuggedContractSession, Maybe, Ref, isDefined, ContractMetadata } from './base'

export type InputBoxType = "parameter" | "storage"

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

export type ValueType = "LIGO" | "Michelson";

export const getEntrypoint = (
		context: ExtensionContext,
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

			const currentFilePath = vscode.window.activeTextEditor?.document.uri.fsPath
			const entrypoints: QuickPickItem[] = currentFilePath && debuggedContract.ref.entrypoints.map(label => ({ label }));

			const currentKey: Maybe<string> = currentFilePath && "quickpick_entrypoint_" + currentFilePath;
			const previousVal: Maybe<string> = currentKey && context.workspaceState.get<string>(currentKey);

			if (entrypoints.length <= 1) {
				if (entrypoints.length === 0) {
					throw new Error("Given contract doesn't have any entrypoints");
				}
				state.ref.pickedEntrypoint = entrypoints[0].label;
				return;
			}

			let activeItem: Maybe<QuickPickItem>;
			if (isDefined(previousVal)) {
				for (let entrypoint of entrypoints) {
					if (entrypoint.label === previousVal) {
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
				if (isDefined(currentKey)) {
					context.workspaceState.update(currentKey, pick.label);
				}

				state.ref.pickedEntrypoint = pick.label;
			}
		}

		const result: State =
			await MultiStepInput.run(
				(input: MultiStepInput<State>, state: Ref<Partial<State>>) => askForEntrypoint(input, state)
			) as State;

		if (isDefined(result.pickedEntrypoint)) {
			await getContractMetadata(result.pickedEntrypoint);
			return result.pickedEntrypoint;
		}
}

export const getParameterOrStorage = (
		context: ExtensionContext,
		validateInput: (inputType: InputBoxType, valueType: ValueType) => (value: string) => Promise<Maybe<string>>,
		inputBoxType: InputBoxType,
		placeHolder: string,
		prompt: string,
		debuggedContract: Ref<DebuggedContractSession>
	) => async (_config: any): Promise<Maybe<string>> => {

	const totalSteps = 1;

	class SwitchButton implements vscode.QuickInputButton {
		public typ: ValueType;

		constructor(public iconPath: vscode.Uri, public tooltip: ValueType) {
			this.typ = tooltip;
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

		const currentFilePath = vscode.window.activeTextEditor?.document.uri.fsPath
		let currentKey: Maybe<string> = currentFilePath && "quickpick_" + inputBoxType + '_' + currentFilePath
		let placeholderExtra: string = ''
		let michelsonType: string = ''
		switch (inputBoxType) {
			case "parameter":
				michelsonType = debuggedContract.ref.contractMetadata.parameterMichelsonType
				// Consider picked entrypoint in the key to remember value depending on an entrypoint
				const entrypoint = debuggedContract.ref.pickedMichelsonEntrypoint
				if (entrypoint) {
					// rewrite currentKey with entrypoint specific one
					currentKey =
						currentFilePath &&
						"quickpick_" + inputBoxType +
						(entrypoint ? + '_' + entrypoint : '') +
						'_' + currentFilePath
					placeholderExtra = " for '" + entrypoint + "' entrypoint"
				}
				break

			case "storage":
				michelsonType = debuggedContract.ref.contractMetadata.storageMichelsonType
				break;
		}

		const previousVal = currentKey && context.workspaceState.get<string>(currentKey)
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

		if (isDefined(input.getCurrentValue()) && isDefined(currentKey)) {
			context.workspaceState.update(currentKey, input.getCurrentValue());
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
			return (input: MultiStepInput<State>, state: Ref<Partial<State>>) => askValue(input, state);
		} else {
			state.ref.value = pick;
		}
	}

	const result: State =
		await MultiStepInput.run(
			(input: MultiStepInput<State>, state) => askValue(input, state),
			{ currentSwitch: SwitchButton.LigoSwitch }
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
		const input = new MultiStepInput();
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
				let validating = validate('');
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
						if (!(await validate(value))) {
							resolve(value);
						}
						input.enabled = true;
						input.busy = false;
					}),
					input.onDidChangeValue(async text => {
						const current = validate(text);
						validating = current;
						const validationMessage = await current;
						if (current === validating) {
							input.validationMessage = validationMessage;
						}
					}),
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
}
