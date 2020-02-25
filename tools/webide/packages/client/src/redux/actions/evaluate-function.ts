import { Dispatch } from 'redux';

import { getErrorMessage, runFunction } from '../../services/api';
import { AppState } from '../app';
import { DoneLoadingAction, UpdateLoadingAction } from '../loading';
import { ChangeOutputAction } from '../result';
import { CancellableAction } from './cancellable';

export class EvaluateFunctionAction extends CancellableAction {
  getAction() {
    return async (dispatch: Dispatch, getState: () => AppState) => {
      const { editor, evaluateFunction: evaluateFunctionState } = getState();

      dispatch({
        ...new UpdateLoadingAction(
          `Evaluating ${evaluateFunctionState.entrypoint} ${evaluateFunctionState.parameters}...`
        )
      });

      try {
        const result = await runFunction(
          editor.language,
          editor.code,
          evaluateFunctionState.entrypoint,
          evaluateFunctionState.parameters
        );
        if (this.isCancelled()) {
          return;
        }
        dispatch({ ...new ChangeOutputAction(result.output) });
      } catch (ex) {
        if (this.isCancelled()) {
          return;
        }
        dispatch({
          ...new ChangeOutputAction(`Error: ${getErrorMessage(ex)}`)
        });
      }

      dispatch({ ...new DoneLoadingAction() });
    };
  }
}
