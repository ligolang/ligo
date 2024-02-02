export class UnknownLigoDialectExtensionException extends Error {
  extension: string

  constructor(extension) {
    super(`Unknown LIGO dialect extension: ${extension}`)
    this.extension = extension
  }
}

export class UnknownCommandTypeException extends Error {
  constructor() {
    super('Encountered Unknown command type')
  }
}

export class ExecutionException extends Error {
  exception: Error

  constructor(exception) {
    super(`LIGO binary execution resulted in error: ${exception.message}`)
    this.exception = exception
  }
}

export class InvalidChoiceException extends Error {
  chosenOption: string

  possibleOptions: string[]

  constructor(chosenOption, possibleOptions: string[]) {
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

export class UserInterruptionException extends Error {
  constructor() {
    super('VSCode textbox was closed by user, without entering any data')
  }
}

export class NoContractPathException extends Error {
  activeTextEditor: string

  constructor(activeTextEditor) {
    super('LIGO contract not found in an active text window')
    this.activeTextEditor = activeTextEditor
  }
}
