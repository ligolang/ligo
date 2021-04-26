export enum Language {
  PascaLigo = 'pascaligo',
  CameLigo = 'cameligo',
  ReasonLIGO = 'reasonligo'
}

export enum CommandType {
  Compile = 'compile',
  DryRun = 'dry-run',
  EvaluateValue = 'evaluate-value',
  EvaluateFunction = 'evaluate-function',
  Deploy = 'deploy',
  GenerateDeployScript = 'generate-deploy-script',
  CompileFunction = 'compile-function'
}

export enum Tool {
  TezosClient = 'tezos-client'
}
