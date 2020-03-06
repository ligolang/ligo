import React from 'react';
import { useSelector } from 'react-redux';
import styled, { css } from 'styled-components';

import { AppState } from '../../redux/app';
import { LoadingState } from '../../redux/loading';
import { ResultState } from '../../redux/result';
import { Command } from '../../redux/types';
import { CompileOutputPane } from './compile-output-pane';
import { DeployOutputPane } from './deploy-output-pane';
import { Loading } from './loading';
import { OutputPane } from './output-pane';

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

export const OutputTab = (props: {
  selected?: boolean;
  onCancel?: () => void;
}) => {
  const command = useSelector<AppState, ResultState['command']>(
    state => state.result.command
  );
  const loading = useSelector<AppState, LoadingState['loading']>(
    state => state.loading.loading
  );

  const renderResult = () => {
    if (loading) {
      return <Loading onCancel={props.onCancel}></Loading>;
    } else if (command === Command.Compile) {
      return <CompileOutputPane></CompileOutputPane>;
    } else if (command === Command.Deploy) {
      return <DeployOutputPane></DeployOutputPane>;
    }

    return <OutputPane></OutputPane>;
  };

  return <Container visible={props.selected}>{renderResult()}</Container>;
};
