// SPDX-FileCopyrightText: 2020 Tocqueville Group
// SPDX-License-Identifier: LicenseRef-MIT-TQ
//
// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA

import * as vscode from 'vscode';

export type Maybe<T> = T | undefined

// Make from an object reference
export type Ref<T> = {
	ref: T
}

export function isDefined<T>(x: T | undefined | null): x is T {
	return x !== null && x !== undefined
}

export interface MichelsonEntrypoints {
	[entrypoint: string]: string
}

export interface ContractMetadata {
	parameterMichelsonType: string
	storageMichelsonType: string
	michelsonEntrypoints: MichelsonEntrypoints
}

export type ContractMetadataFetcher = (file: string, logDir: string) => Promise<ContractMetadata>

export interface DebuggedContractSession {
	pickedMichelsonEntrypoint?: string
	logDir?: string
	contractMetadata?: ContractMetadata
}
