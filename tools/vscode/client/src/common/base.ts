// SPDX-FileCopyrightText: 2020 Tocqueville Group
// SPDX-License-Identifier: LicenseRef-MIT-TQ
//
// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA

/** The type of validation in an input box.
 *
 * `undefined` would stand for validation pass, and a `string` would mean
 * the reason of a failure.
 */
export type InputValidationResult = Maybe<string>

export type Maybe<T> = T | undefined

// Make from an object reference
export type Ref<T> = {
  ref: T
}

export function isDefined<T>(x: T | undefined | null): x is T {
  return x !== null && x !== undefined
}

// Type if input box that initializ
export type InputBoxType = "parameter" | "storage"

export type InputValueLang = "LIGO" | "Michelson";
