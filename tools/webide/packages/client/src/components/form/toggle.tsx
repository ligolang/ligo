import { faCheck } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import React, { useState } from 'react';
import styled, { css } from 'styled-components';

const Container = styled.div<{ checked: boolean }>`
  position: relative;
  height: 2em;
  width: 3.5em;
  border-radius: 1em;
  background-color: var(--blue_trans1);
  border: 1px solid var(--blue);
  transition: background-color 0.2s ease-in;

  ${props =>
    props.checked &&
    css`
      background-color: var(--blue);
    `};
`;

const Button = styled.div<{ checked: boolean }>`
  display: flex;
  justify-content: center;
  align-items: center;
  position: absolute;

  height: 2em;
  width: 2em;
  background-color: white;
  border-radius: 50%;
  cursor: pointer;
  right: calc(1.5em);
  transition: right 0.2s ease-in;

  ${props =>
    props.checked &&
    css`
      right: 0;
    `};
`;

const CheckIcon = ({ visible, ...props }: { visible: boolean }) => (
  <FontAwesomeIcon {...props} icon={faCheck}></FontAwesomeIcon>
);

const Check = styled(CheckIcon)`
  position: absolute;
  pointer-events: none;
  opacity: 1;
  transition: opacity 0.2s ease-in;
  color: var(--blue);

  ${props =>
    !props.visible &&
    css`
      opacity: 0;
    `}
`;

export const ToggleComponent = (props: {
  checked: boolean;
  onChanged: (value: boolean) => void;
  className?: string;
}) => {
  const [isChecked, setChecked] = useState(props.checked);

  return (
    <Container className={props.className} checked={isChecked}>
      <Button
        checked={isChecked}
        onClick={() => {
          const newState = !isChecked;

          setChecked(newState);
          props.onChanged(newState);
        }}
      >
        <Check visible={isChecked}></Check>
      </Button>
    </Container>
  );
};
