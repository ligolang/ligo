// SPDX-FileCopyrightText: 2020 Tocqueville Group
// SPDX-License-Identifier: LicenseRef-MIT-TQ
//
// SPDX-FileCopyrightText: 2022 Oxhead Alpha
// SPDX-License-Identifier: LicenseRef-MIT-OA

/**
 * The type of validation in an input box.
 *
 * `undefined` would stand for validation pass, and a `string` would mean
 * the reason of a failure.
 */
export type InputValidationResult = Maybe<string>

/** Shorthand for writing that an object might be `undefined`. */
export type Maybe<T> = T | undefined

/** Make an object reference. */
export type Ref<T> = {
  ref: T
}

/**
 * Checks whether the given value is not `null` or `undefined`, providing
 * evidence that the value `x` has type `T`.
 */
export function isDefined<T>(x: T | undefined | null): x is T {
  return x !== null && x !== undefined
}

/** Type of an input box for parameter or storage validation. */
export type InputBoxType = "parameter" | "storage"

/** Type of an input box indicating that the language is LIGO or Michelson. */
export type InputValueLang = "LIGO" | "Michelson";
