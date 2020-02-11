import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../redux/app';
import { ChangeSelectedAction, ExamplesState } from '../redux/examples';
import { getExample } from '../services/api';

const bgColor = 'transparent';
const borderSize = '5px';
const verticalPadding = '0.8em';

const Container = styled.div`
  flex: 0.5;
  display: flex;
  flex-direction: column;
`;

const MenuItem = styled.div<{ selected: boolean }>`
  padding: ${verticalPadding} 0 ${verticalPadding} 1em;
  height: 1.5em;
  display: flex;
  align-items: center;
  cursor: pointer;
  background-color: ${props =>
    props.selected ? 'var(--blue_trans1)' : bgColor};
  border-left: ${`${borderSize} solid ${bgColor}`};
  border-left-color: ${props => (props.selected ? 'var(--blue)' : bgColor)};

  :first-child {
    margin-top: ${props => (props.selected ? '0' : `-${borderSize}`)};
  }

  :hover {
    background-color: ${props =>
      props.selected ? 'var(--blue_trans1)' : 'var(--blue_trans2)'};
    border-left: ${`${borderSize} solid ${bgColor}`};
    border-left-color: ${props =>
      props.selected ? 'var(--blue)' : 'transparent'};
    :first-child {
      margin-top: ${props => (props.selected ? '0' : `-${borderSize}`)};
      padding-top: ${props =>
        props.selected
          ? `${verticalPadding}`
          : `calc(${verticalPadding} - ${borderSize})`};
      border-top: ${props =>
        props.selected ? '' : `${borderSize} solid var(--blue_opaque1)`};
    }
  }
`;

const MenuContainer = styled.div`
  display: flex;
  flex-direction: column;
  overflow-y: auto;
  height: var(--content_height);
  box-sizing: border-box;
`;

const Header = styled.div<{ firstChildSelected: boolean }>`
  border-bottom: ${props =>
    props.firstChildSelected ? '' : '5px solid var(--blue_trans1)'};
  min-height: 2.5em;
  padding: 0 10px;
  display: flex;
  align-items: center;
`;

export const Examples = () => {
  const examples = useSelector<AppState, ExamplesState['list']>(
    (state: AppState) => state.examples.list
  );
  const selectedExample = useSelector<AppState, ExamplesState['selected']>(
    (state: AppState) => state.examples.selected
  );
  const dispatch = useDispatch();

  return (
    <Container>
      <Header
        firstChildSelected={
          !!selectedExample && examples[0].id === selectedExample.id
        }
      >
        <span>Examples</span>
      </Header>
      <MenuContainer>
        {examples.map(example => {
          return (
            <MenuItem
              id={example.id}
              key={example.id}
              selected={!!selectedExample && example.id === selectedExample.id}
              onClick={async () => {
                const response = await getExample(example.id);

                dispatch({ ...new ChangeSelectedAction(response) });
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
