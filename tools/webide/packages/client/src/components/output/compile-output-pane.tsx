import React, { useRef } from 'react';
import { useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ResultState } from '../../redux/result';
import { OutputToolbarComponent } from './output-toolbar';

const Container = styled.div<{ visible?: boolean }>`
  display: flex;
  flex-direction: column;
  height: 100%;
`;

const Output = styled.div`
  flex: 1;
  padding: 0.5em;
  display: flex;
  overflow: scroll;
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

function downloadOutput(output: string) {
  const anchor = document.createElement('a');
  anchor.setAttribute(
    'href',
    `data:text/plain;charset=utf-8,${encodeURIComponent(output)}`
  );
  anchor.setAttribute('download', 'output.txt');

  anchor.style.display = 'none';
  document.body.appendChild(anchor);
  anchor.click();
  document.body.removeChild(anchor);
}

export const CompileOutputPane = () => {
  const output = useSelector<AppState, ResultState['output']>(
    state => state.result.output
  );

  const preRef = useRef<HTMLPreElement>(null);

  return (
    <Container>
      <OutputToolbarComponent
        onCopy={() => copyOutput(preRef.current)}
        onDownload={() => downloadOutput(output)}
      ></OutputToolbarComponent>
      <Output id="output">
        <Pre ref={preRef}>{output}</Pre>
      </Output>
    </Container>
  );
};
