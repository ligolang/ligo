import { combineReducers } from 'redux';

import Command, { CommandState } from './command';
import Compile, { CompileState } from './compile';
import Deploy, { DeployState } from './deploy';
import { DryRunState, DryRun } from './dry-run';
import Editor, { EditorState } from './editor';
import EvaluateFunction, { EvaluateFunctionState } from './evaluate-function';
import EvaluateValue, { EvaluateValueState } from './evaluate-value';
import Examples, { ExamplesState } from './examples';
import GenerateDeployScript, { GenerateDeployScriptState } from './generate-deploy-script';
import Loading, { LoadingState } from './loading';
import Result, { ResultState } from './result';
import Share, { ShareState } from './share';
import Version, { VersionState } from './version';

export interface AppState {
  Version: VersionState;
  Editor: EditorState;
  Share: ShareState;
  Compile: CompileState;
  DryRun: DryRunState;
  Deploy: DeployState;
  EvaluateFunction: EvaluateFunctionState;
  EvaluateValue: EvaluateValueState;
  GenerateDeployScript: GenerateDeployScriptState;
  result: ResultState;
  Command: CommandState;
  Examples: ExamplesState;
  Loading: LoadingState;
}

const reducer = combineReducers({
  Editor,
  Share,
  Compile,
  DryRun,
  Deploy,
  EvaluateFunction,
  EvaluateValue,
  GenerateDeployScript,
  Result,
  Command,
  Examples,
  Loading,
  Version
});

export default reducer