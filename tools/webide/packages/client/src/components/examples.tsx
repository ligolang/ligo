import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../redux/app';
import { ChangeDirtyAction, EditorState } from '../redux/editor';
import { ChangeSelectedAction, ExamplesState } from '../redux/examples';
import { getExample } from '../services/api';

const Container = styled.div`
  flex: 0.5;
  display: flex;
  flex-direction: column;
  min-width: 0;
`;

const Header = styled.div`
  min-height: 2.5em;
  display: flex;
  align-items: center;
  font-weight: 600;
`;

const MenuContainer = styled.div`
  display: flex;
  flex-direction: column;
  overflow-y: auto;
  height: var(--content_height);
  font-size: 0.8em;
`;

const MenuItem = styled.span`
  height: 1em;
  padding: 0.6em;
  cursor: pointer;
  background-color: transparent;

  overflow: hidden;
  white-space: nowrap;
  text-overflow: ellipsis;

  :hover {
    background-color: var(--blue_trans2);
  }
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
