import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../redux/app';
import { ChangeTitleAction } from '../redux/editor';
import { EditableTitleComponent } from './editable-title';
import { MonacoComponent } from './monaco';
import { ShareComponent } from './share';
import { SyntaxSelectComponent } from './syntax-select';

const Container = styled.div`
  flex: 2;
`;

const Header = styled.div`
  flex: 1;
  display: flex;
  justify-content: flex-start;
  align-items: center;

  min-height: 2.5em;
  border-bottom: 5px solid var(--blue_trans1);
`;

const Subheader = styled.div`
  flex: 1;
  display: flex;
  justify-content: space-between;
  align-items: center;

  background: var(--blue_trans1);
  border: 5px solid rgba(0, 0, 0, 0);
  border-top: none;
  padding: 0 10px;
`;

export const EditorComponent = () => {
  const dispatch = useDispatch();
  const title = useSelector<AppState, string>(state => state.editor.title);

  return (
    <Container>
      <Header>
        <ShareComponent></ShareComponent>
      </Header>
      <Subheader>
        <EditableTitleComponent
          id="editor-title"
          title={title}
          onChanged={value => {
            dispatch({ ...new ChangeTitleAction(value) });
          }}
        ></EditableTitleComponent>
        <SyntaxSelectComponent></SyntaxSelectComponent>
      </Subheader>
      <MonacoComponent></MonacoComponent>
    </Container>
  );
};
