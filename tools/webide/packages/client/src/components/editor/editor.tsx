import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ChangeTitleAction } from '../../redux/editor';
import { ShareComponent } from '../share';
import { EditableTitleComponent } from './editable-title';
import { MonacoComponent } from './monaco';
import { SyntaxSelectComponent } from './syntax-select';

const Container = styled.div`
  flex: 2;
`;

const Header = styled.div`
  flex: 1;
  display: flex;
  justify-content: space-between;
  align-items: center;

  background: var(--blue_trans1);
  border: 5px solid rgba(0, 0, 0, 0);
  padding: 0 10px;
`;

const LeftActions = styled.div`
  display: flex;
`;

const StyledEditableTitleComponent = styled(EditableTitleComponent)`
  margin-left: 20px;
`;

export const EditorComponent = () => {
  const dispatch = useDispatch();
  const title = useSelector<AppState, string>(state => state.editor.title);

  return (
    <Container>
      <Header>
        <LeftActions>
          <ShareComponent></ShareComponent>
          <StyledEditableTitleComponent
            id="editor-title"
            title={title}
            onChanged={value => {
              dispatch({ ...new ChangeTitleAction(value) });
            }}
          ></StyledEditableTitleComponent>
        </LeftActions>
        <SyntaxSelectComponent></SyntaxSelectComponent>
      </Header>
      <MonacoComponent></MonacoComponent>
    </Container>
  );
};
