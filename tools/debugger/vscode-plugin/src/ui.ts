// SPDX-FileCopyrightText: 2020 Tocqueville Group
// SPDX-License-Identifier: LicenseRef-MIT-TQ
//
// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA

// Useful UI elements which is used to make the plugin more pleasant to use.

import * as vscode from 'vscode';
import { ExtensionContext, QuickPickItem } from 'vscode';
import { DebuggedContractSession, Maybe, Ref, isDefined, ContractMetadata } from './base'

export type InputBoxType = "parameter" | "storage"

// Create input box which remembers previously
// inputted value in workspace storage.
export const createRememberingInputBox = (
	context: ExtensionContext,
	validateInput: (inputType: InputBoxType) => (value: string) => Promise<Maybe<string>>,
	inputBoxType: InputBoxType,
	placeHolder: string,
	prompt: string,
	debuggedContract: Ref<DebuggedContractSession>
) => (_config: any): Thenable<Maybe<string>> => {

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
	return vscode.window.showInputBox({
		placeHolder: placeHolder + placeholderExtra,
		prompt: prompt,
		value: defaultValue.value,
		valueSelection: defaultValue.selection,
		validateInput: validateInput(inputBoxType),
		// Keep input box open if focus is moved out
		ignoreFocusOut: true
	}).then(newVal => {
		// Preserve new parameter value to show it next time
		if (isDefined(newVal) && isDefined(currentKey)) {
			context.workspaceState.update(currentKey, newVal)
		}
		return newVal
	})
}

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
