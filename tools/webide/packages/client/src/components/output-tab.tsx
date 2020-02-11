import React, { useEffect, useRef, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { PushSpinner } from 'react-spinners-kit';
import styled, { css } from 'styled-components';

import { AppState } from '../redux/app';
import { CommandState } from '../redux/command';
import { DoneLoadingAction, LoadingState } from '../redux/loading';
import { ResultState } from '../redux/result';

const Container = styled.div<{ visible?: boolean }>`
  position: absolute;
  box-sizing: border-box;
  width: 100%;
  height: 100%;

  font-family: Menlo, Monaco, 'Courier New', monospace;
  overflow: scroll;
  display: flex;

  transform: translateX(100%);
  transition: transform 0.2s ease-in;

  ${props =>
    props.visible &&
    css`
      transform: translateX(0px);
    `}
`;

const CancelButton = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
  color: white;
  background-color: #fc683a;
  cursor: pointer;
  user-select: none;
  margin: 1em;
  padding: 0.5em 1em;
`;

const Output = styled.div`
  flex: 1;
  padding: 0.8em;
  display: flex;

  /* This font size is used to calcuate spinner size */
  font-size: 1em;
`;

const LoadingContainer = styled.div`
  flex: 1;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
`;

const LoadingMessage = styled.div`
  padding: 1em 0;
`;

const Pre = styled.pre`
  margin: 0;
`;

export const OutputTabComponent = (props: {
  selected?: boolean;
  onCancel?: () => void;
}) => {
  const output = useSelector<AppState, ResultState['output']>(
    state => state.result.output
  );
  const contract = useSelector<AppState, ResultState['contract']>(
    state => state.result.contract
  );

  const loading = useSelector<AppState, LoadingState>(state => state.loading);

  const dispatchedAction = useSelector<
    AppState,
    CommandState['dispatchedAction']
  >(state => state.command.dispatchedAction);

  const dispatch = useDispatch();

  const outputRef = useRef(null);
  const [spinnerSize, setSpinnerSize] = useState(50);

  useEffect(() => {
    const htmlElement = (outputRef.current as unknown) as HTMLElement;
    const fontSize = window
      .getComputedStyle(htmlElement, null)
      .getPropertyValue('font-size');

    setSpinnerSize(parseFloat(fontSize) * 3);
  }, [setSpinnerSize]);

  return (
    <Container visible={props.selected}>
      <Output id="output" ref={outputRef}>
        {loading.loading && (
          <LoadingContainer>
            <PushSpinner size={spinnerSize} color="#fedace" />
            <LoadingMessage>{loading.message}</LoadingMessage>
            <CancelButton
              onClick={() => {
                if (dispatchedAction) {
                  dispatchedAction.cancel();
                }

                dispatch({ ...new DoneLoadingAction() });

                if (props.onCancel) {
                  props.onCancel();
                }
              }}
            >
              Cancel
            </CancelButton>
          </LoadingContainer>
        )}
        {!loading.loading &&
          ((output.length !== 0 && <Pre>{output}</Pre>) ||
            (contract.length !== 0 && (
              <span>
                The contract was successfully deployed to the babylonnet test
                network.
                <br />
                <br />
                The address of your new contract is: <i>{contract}</i>
                <br />
                <br />
                View your new contract using{' '}
                <a
                  target="_blank"
                  rel="noopener noreferrer"
                  href={`https://better-call.dev/babylon/${contract}`}
                >
                  Better Call Dev
                </a>
                !
              </span>
            )))}
      </Output>
    </Container>
  );
};
