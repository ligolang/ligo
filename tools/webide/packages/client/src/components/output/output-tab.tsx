import React from 'react';
import { useSelector } from 'react-redux';
import styled, { css } from 'styled-components';

import { AppState } from '../../redux/app';
import { LoadingState } from '../../redux/loading';
import { ResultState } from '../../redux/result';
import { Command } from '../../redux/types';
import { Statusbar } from '../statusBar';
import { CompileOutputPane } from './compile-output-pane';
import { DeployOutputPane } from './deploy-output-pane';
import { GenerateDeployScriptOutputPane } from './generate-deploy-script-output-pane';
import { Loading } from './loading';
import { OutputPane } from './output-pane';

const Container = styled.div<{ visible?: boolean }>`
  z-index: 2;
  box-sizing: border-box;
  width: -webkit-fill-available;
  height: 100%;
  minHeight: "50px";
  overflow-x: hidden; 
  overflow-y: auto; 
  background-color: white;

  font-family: Menlo, Monaco, 'Courier New', monospace;
  display: flex;
  flex-direction: column;

 
  transition: transform 0.2s ease-in;

  ${props =>
    props.visible ?
    css`
      transform: translateX(0px);
    `
    : css`
    visibility: hidden;
    transform: translateX(0px);
  `}
`;

export const OutputTab = (props: {
  selected?: boolean;
  onCancel?: () => void;
}) => {
  let visible = props.selected;
  const command = useSelector<AppState, ResultState['command']>(
    state => state.result.command
  );
  const loading = useSelector<AppState, LoadingState['loading']>(
    state => state.loading.loading
  );
  const output = useSelector<AppState, ResultState['output']>(
    state => state.result.output
  );

  if( loading || output ) {
    visible = true
  }

  const error = useSelector<AppState, ResultState['error']>(
    state => state.result.error
  );
  
  const renderResult = () => {
    if (loading) {
      return <><Statusbar error={false} /><Loading onCancel={props.onCancel}></Loading></>;
    } else if (!output) {
      return <></>;
    } else if (command === Command.Compile) {
      return <CompileOutputPane></CompileOutputPane>;
    } else if (command === Command.Deploy) {
      return <><Statusbar error={error} /><DeployOutputPane></DeployOutputPane></>;
    } else if (command === Command.GenerateDeployScript) {
      return <GenerateDeployScriptOutputPane></GenerateDeployScriptOutputPane>;
    }

    return <><Statusbar error={error}/><OutputPane></OutputPane></>;
  };

  return <Container visible={visible}>{renderResult()}</Container>;
};
