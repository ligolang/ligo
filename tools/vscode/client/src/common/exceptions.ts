/**
 * Indicates that the file extension does not match a known LIGO file extension.
 */
export class UnknownLigoDialectExtensionException extends Error {
  extension: string

  constructor(extension: string) {
    super(`Unknown LIGO dialect extension: ${extension}`)
    this.extension = extension
  }
}

/** Indicates that it was not possible to succesfully run a command. */
export class ExecutionException extends Error {
  exception: Error

  constructor(exception: Error) {
    super(`LIGO binary execution resulted in error: ${exception.message}`)
    this.exception = exception
  }
}

/**
 * Indicates that an input choice from the user does not match any of the
 * provided choices for the given context.
 */
export class InvalidChoiceException extends Error {
  chosenOption: string

  possibleOptions: string[]

  constructor(chosenOption: string, possibleOptions: string[]) {
    let startLine = `Your chosen option '${chosenOption}' is not possible. Please, choose one
                    of these options: \n`

    possibleOptions.forEach((element) => {
      startLine += `- ${element} \n`;
    })
    super(startLine)
    this.chosenOption = chosenOption
    this.possibleOptions = possibleOptions
  }
}

/** Indicates that a textbox was closed by the user. */
export class UserInterruptionException extends Error {
  constructor() {
    super('VSCode textbox was closed by user, without entering any data')
  }
}

/**
 * Indicates that a contract that was previously active in the editor could not
 * be found any more (perhaps it was closed or deleted).
 */
export class NoContractPathException extends Error {
  activeTextEditor: string

  constructor(activeTextEditor: string) {
    super('LIGO contract not found in an active text window')
    this.activeTextEditor = activeTextEditor
  }
}

/**
 * Indicates that the extension was not able to query the GitLab releases page
 * for new LIGO releases.
 */
export class NoReleasesAccess extends Error {
  constructor() {
    super('Could not check for the recent LIGO release')
  }
}
