import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../redux/app';
import { ChangeDirtyAction, EditorState } from '../redux/editor';
import { ChangeSelectedAction, ExamplesState } from '../redux/examples';
import { getExample } from '../services/api';

const bgColor = 'transparent';
const borderSize = '5px';
const verticalPadding = '0.6em';

const Container = styled.div`
  flex: 0.5;
  display: flex;
  flex-direction: column;
`;

const MenuItem = styled.div<{ selected?: boolean }>`
  padding: ${verticalPadding} 0 ${verticalPadding} 1em;
  height: 1em;
  display: flex;
  align-items: center;
  cursor: pointer;
  background-color: ${props =>
    props.selected ? 'var(--blue_trans1)' : bgColor};
  border-left: ${`${borderSize} solid ${bgColor}`};
  border-left-color: ${props => (props.selected ? 'var(--blue)' : bgColor)};

  :hover {
    background-color: ${props =>
      props.selected ? 'var(--blue_trans1)' : 'var(--blue_trans2)'};
    border-left: ${`${borderSize} solid ${bgColor}`};
    border-left-color: ${props =>
      props.selected ? 'var(--blue)' : 'transparent'};
  }
`;

const MenuContainer = styled.div`
  display: flex;
  flex-direction: column;
  overflow-y: auto;
  height: var(--content_height);
  box-sizing: border-box;
`;

const Header = styled.div`
  min-height: 2.5em;
  padding: 0 10px;
  display: flex;
  align-items: center;
  font-weight: 600;
`;

export const Examples = () => {
  const examples = useSelector<AppState, ExamplesState['list']>(
    (state: AppState) => state.examples.list
  );
  const editorDirty = useSelector<AppState, EditorState['dirty']>(
    (state: AppState) => state.editor.dirty
  );

  const dispatch = useDispatch();

  return (
    <Container>
      <Header>Examples</Header>
      <MenuContainer>
        {examples.map(example => {
          return (
            <MenuItem
              id={example.id}
              key={example.id}
              onClick={async () => {
                const response = await getExample(example.id);

                if (
                  !editorDirty ||
                  window.confirm(
                    'Are you sure you want to navigate away? Data you have entered will be lost.\n\nPress OK to continue or Cancel to stay on the current page.\n\n'
                  )
                ) {
                  dispatch({ ...new ChangeSelectedAction(response) });
                  dispatch({ ...new ChangeDirtyAction(false) });
                }
              }}
            >
              {example.name}
            </MenuItem>
          );
        })}
      </MenuContainer>
    </Container>
  );
};
