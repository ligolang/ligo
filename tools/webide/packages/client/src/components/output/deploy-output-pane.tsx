import React from 'react';
import { useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ResultState } from '../../redux/result';

const Container = styled.div<{ visible?: boolean }>`
  display: flex;
  flex-direction: column;
  height: 100%;
`;

const Output = styled.div`
  flex: 1;
  padding: 0.5em 0.5em 0 0.5em;
  display: flex;
  flex-direction: column;
  overflow: auto;
`;

const Pre = styled.pre`
  padding: 0.5em;
  margin: 0 -0.5em;
  overflow: scroll;
  height: 100%;
`;

export const DeployOutputPane = () => {
  const output = useSelector<AppState, ResultState['output']>(
    state => state.result.output
  );
  const contract = useSelector<AppState, ResultState['contract']>(
    state => state.result.contract
  );

  return (
    <Container>
      <Output id="output">
        {contract && (
          <div>
            The contract was successfully deployed to the babylonnet test
            network.
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
            <br />
            <br />
            <b>The address of your new contract is: </b>
            <i>{contract}</i>
            <br />
            <br />
            <b>The initial storage of your contract is: </b>
          </div>
        )}
        {output && <Pre>{output}</Pre>}
      </Output>
    </Container>
  );
};
