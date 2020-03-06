import { faCaretDown } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import React, { useState } from 'react';
import OutsideClickHandler from 'react-outside-click-handler';
import { useDispatch, useSelector } from 'react-redux';
import styled, { css } from 'styled-components';

import { AppState } from '../../redux/app';
import { ChangeLanguageAction, EditorState } from '../../redux/editor';
import { Language } from '../../redux/types';
import { Tooltip } from '../tooltip';

const Container = styled.div`
  display: flex;
  position: relative;
  z-index: 2;
  min-width: 10em;
`;

const Header = styled.div`
  cursor: pointer;
  user-select: none;

  flex: 1;
  display: flex;
  height: 2em;
  padding: 0 0.5em;

  border: 1px solid var(--blue_trans1);
`;

const Label = styled.div`
  flex: 1;
  display: flex;
  align-items: center;
  justify-content: space-between;
`;

const ArrowIcon = ({ rotate, ...props }: { rotate: boolean }) => (
  <FontAwesomeIcon {...props} icon={faCaretDown} size="lg"></FontAwesomeIcon>
);

const Arrow = styled(ArrowIcon)`
  margin-left: 0.5em;
  pointer-events: none;
  color: var(--blue_trans1);
  transition: transform 0.15s ease-in;

  ${(props: { rotate: boolean }) =>
    props.rotate &&
    css`
      transform: rotate(180deg);
    `};
`;

const List = styled.ul`
  position: absolute;
  list-style-type: none;
  background-color: white;
  width: 100%;
  margin: 0;
  padding: 0;
  box-shadow: 1px 3px 10px 0px rgba(153, 153, 153, 0.4);
  border-radius: 3px;

  visibility: hidden;
  opacity: 0;
  transition: opacity 0.15s ease-in;

  ${(props: { visible: boolean }) =>
    props.visible &&
    css`
      visibility: visible;
      opacity: 1;
    `}
`;

const Option = styled.li`
  cursor: pointer;
  user-select: none;

  display: flex;
  align-items: center;
  justify-content: space-between;
  height: 2em;
  padding: 0 0.5em;

  &:first-child {
    border-radius: 3px 3px 0 0;
  }

  &:last-child {
    border-radius: 0 0 3px 3px;
  }

  &:hover {
    background-color: var(--blue_trans2);
    font-weight: 600;
  }
`;

export const SyntaxSelectComponent = () => {
  const OPTIONS = {
    [Language.PascaLigo]: 'PascaLIGO',
    [Language.CameLigo]: 'CameLIGO',
    [Language.ReasonLIGO]: 'ReasonLIGO'
  };

  const moveOptionToTop = (option: Language) => {
    return Object.keys(OPTIONS).reduce((list, entry) => {
      if (entry === option) {
        list.unshift(entry);
      } else {
        list.push(entry as Language);
      }
      return list;
    }, [] as Language[]);
  };

  const language = useSelector<AppState, EditorState['language']>(
    state => state.editor.language
  );
  const dispatch = useDispatch();
  const [opened, open] = useState(false);

  const selectOption = (option: Language) => {
    if (language !== option) {
      dispatch({ ...new ChangeLanguageAction(option) });
    }
    open(false);
  };

  return (
    <Container>
      <OutsideClickHandler onOutsideClick={() => open(false)}>
        <List visible={opened}>
          {moveOptionToTop(language).map(option => (
            <Option
              id={option}
              key={option}
              onClick={() => selectOption(option)}
            >
              <span>{OPTIONS[option]}</span>
            </Option>
          ))}
        </List>
      </OutsideClickHandler>
      <Header id="syntax-select" onClick={() => open(true)}>
        <Label>
          {OPTIONS[language]}
          <Arrow rotate={opened}></Arrow>
        </Label>
        <Tooltip>Select syntax</Tooltip>
      </Header>
    </Container>
  );
};
