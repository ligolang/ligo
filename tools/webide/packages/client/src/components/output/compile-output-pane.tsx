import React, { useRef } from 'react';
import { useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ResultState } from '../../redux/result';
import { OutputToolbarComponent } from './output-toolbar';
import { copyOutput, downloadOutput } from './utils';
import { CombSpinner } from 'react-spinners-kit';

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
  width: -webkit-fill-available;
`;


export const CompileOutputPane = () => {
  var parse = require('shell-quote').parse;
  const output = useSelector<AppState, ResultState['output']>(
    state => state.result.output
  );

  const preRef = useRef<HTMLPreElement>(null);

  return (
    <Container>
      <OutputToolbarComponent
        showTryMichelson={true}
        onCopy={() => copyOutput(preRef.current)}
        onDownload={() => downloadOutput(output)}
      ></OutputToolbarComponent>
      <Output id="output">
        <Pre ref={preRef}>{output}</Pre>
      </Output>
    </Container>
  );
};
