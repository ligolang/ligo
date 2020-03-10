import React from 'react';
import { useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ResultState } from '../../redux/result';

const Container = styled.div<{ visible?: boolean }>`
  display: flex;
  flex-direction: column;
  flex: 1;
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

export const OutputPane = () => {
  const output = useSelector<AppState, ResultState['output']>(
    state => state.result.output
  );

  return (
    <Container>
      <Output id="output">
        <Pre>{output}</Pre>
      </Output>
    </Container>
  );
};
