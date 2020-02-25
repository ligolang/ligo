import React, { useEffect, useRef, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { PushSpinner } from 'react-spinners-kit';
import styled, { css } from 'styled-components';

import { AppState } from '../redux/app';
import { CommandState } from '../redux/command';
import { DoneLoadingAction, LoadingState } from '../redux/loading';
import { ResultState } from '../redux/result';
import { OutputToolbarComponent } from './output-toolbar';

const Container = styled.div<{ visible?: boolean }>`
  position: absolute;
  box-sizing: border-box;
  width: 100%;
  height: 100%;

  font-family: Menlo, Monaco, 'Courier New', monospace;
  display: flex;
  flex-direction: column;

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
  padding: 0 0.5em 0.5em 0.5em;
  display: flex;
  overflow: scroll;
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

function copyOutput(el: HTMLElement | null) {
  if (el) {
    const range = document.createRange();
    range.selectNodeContents(el);

    const selection = window.getSelection();

    if (selection) {
      selection.removeAllRanges();
      selection.addRange(range);
      document.execCommand('copy');
    }
  }
}

function downloadOutput(el: HTMLElement | null) {
  if (el) {
    const anchor = document.createElement('a');
    anchor.setAttribute(
      'href',
      'data:text/plain;charset=utf-8,' + encodeURIComponent(el.innerHTML)
    );
    anchor.setAttribute('download', 'output.txt');

    anchor.style.display = 'none';
    document.body.appendChild(anchor);
    anchor.click();
    document.body.removeChild(anchor);
  }
}

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

  const outputRef = useRef<HTMLDivElement>(null);
  const preRef = useRef<HTMLPreElement>(null);
  const [spinnerSize, setSpinnerSize] = useState(50);

  useEffect(() => {
    const outputEl = (outputRef.current as unknown) as HTMLElement;
    const fontSize = window
      .getComputedStyle(outputEl, null)
      .getPropertyValue('font-size');

    setSpinnerSize(parseFloat(fontSize) * 3);
  }, [setSpinnerSize]);

  return (
    <Container visible={props.selected}>
      {output.length !== 0 && (
        <OutputToolbarComponent
          onCopy={() => copyOutput(preRef.current)}
          onDownload={() => downloadOutput(preRef.current)}
        ></OutputToolbarComponent>
      )}
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
          ((output.length !== 0 && <Pre ref={preRef}>{output}</Pre>) ||
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
