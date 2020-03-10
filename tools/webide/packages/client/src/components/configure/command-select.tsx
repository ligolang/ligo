import { faCaretDown } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import React, { useState } from 'react';
import OutsideClickHandler from 'react-outside-click-handler';
import styled, { css } from 'styled-components';

import { Command } from '../../redux/types';

const Container = styled.div`
  flex: 2;
  display: flex;
  position: relative;
  min-width: 8em;
  z-index: 2;
`;

const Header = styled.div`
  cursor: pointer;
  user-select: none;

  flex: 1;
  display: flex;
  align-items: center;
  justify-content: space-between;
  min-height: 2em;
  padding: 0 0.5em;

  border: 1px solid var(--blue_trans1);
`;

const ArrowIcon = ({ rotate, ...props }: { rotate: boolean }) => (
  <FontAwesomeIcon {...props} icon={faCaretDown} size="lg"></FontAwesomeIcon>
);

const Arrow = styled(ArrowIcon)`
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

export const CommandSelectComponent = (props: {
  selected: Command;
  onChange?: (value: Command) => void;
}) => {
  const OPTIONS = {
    [Command.Compile]: 'Compile',
    [Command.Deploy]: 'Deploy',
    [Command.DryRun]: 'Dry Run',
    [Command.EvaluateFunction]: 'Evaluate Function',
    [Command.EvaluateValue]: 'Evaluate Value'
  };

  const moveOptionToTop = (option: Command) => {
    return Object.keys(OPTIONS).reduce((list, entry) => {
      if (entry === option) {
        list.unshift(entry);
      } else {
        list.push(entry as Command);
      }
      return list;
    }, [] as Command[]);
  };

  const [opened, open] = useState(false);

  const selectOption = (option: Command) => {
    if (props.selected !== option && props.onChange) {
      props.onChange(option);
    }
    open(false);
  };

  return (
    <Container>
      <OutsideClickHandler onOutsideClick={() => open(false)}>
        <List visible={opened}>
          {moveOptionToTop(props.selected).map(option => (
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
      <Header id="command-select" onClick={() => open(true)}>
        <span>{OPTIONS[props.selected]}</span>
        <Arrow rotate={opened}></Arrow>
      </Header>
    </Container>
  );
};
