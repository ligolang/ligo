import { faPencilAlt } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import React, { useEffect, useRef, useState } from 'react';
import styled, { css } from 'styled-components';

import { Tooltip } from './tooltip';

const Container = styled.div`
  display: flex;
  align-items: center;
`;

const Input = styled.input<{ visible?: boolean }>`
  position: absolute;
  border-radius: var(--border_radius);
  opacity: 0;
  height: 2em;
  width: 0;
  border: none;
  font-size: 1em;

  outline: none;
  z-index: 1;

  ${props =>
    props.visible &&
    css`
      padding-left: 0.5em;
      opacity: 1;
      width: 15em;
    `}
`;

const Label = styled.div<{ visible?: boolean }>`
  color: var(--blue);
  opacity: 0;

  ${props =>
    props.visible &&
    css`
      opacity: 1;
    `}
`;

const PencilIcon = ({ ...props }) => (
  <FontAwesomeIcon {...props} icon={faPencilAlt} size="sm"></FontAwesomeIcon>
);

const Pencil = styled(PencilIcon)<{ visible: boolean }>`
  margin-left: 10px;
  cursor: pointer;
  color: var(--label_foreground);
  opacity: 0;

  :hover {
    opacity: 1;
  }

  ${props =>
    props.visible &&
    css`
      opacity: 0.5;
    `}
`;

export const EditableTitleComponent = (props: {
  id?: string;
  title: string;
  onChanged?: (value: string) => void;
  className?: string;
}) => {
  const [newTitle, setNewTitle] = useState(props.title);
  const [showInput, setShowInput] = useState(false);
  const inputEl = useRef<HTMLInputElement>(null);

  const notifyChanged = () => {
    if (props.onChanged && props.title !== newTitle) {
      props.onChanged(newTitle);
    }

    setShowInput(false);
  };

  useEffect(() => {
    setNewTitle(props.title);
  }, [props.title]);

  return (
    <Container id={props.id} className={props.className}>
      <Input
        ref={inputEl}
        visible={showInput}
        value={newTitle}
        onChange={event => setNewTitle(event.target.value)}
        onBlur={_ => notifyChanged()}
        onKeyDown={event => {
          if (event.key === 'Enter') {
            notifyChanged();
          } else if (event.key === 'Escape') {
            setNewTitle(props.title);
            setShowInput(false);
          }
        }}
      ></Input>
      <Label visible={!showInput}>{newTitle ? newTitle : 'Untitled'}</Label>
      <div>
        <Pencil
          visible={!showInput}
          onClick={() => {
            if (inputEl.current) {
              inputEl.current.select();
              inputEl.current.setSelectionRange(0, 99999);
              setShowInput(true);
            }
          }}
        ></Pencil>
        <Tooltip>Rename</Tooltip>
      </div>
    </Container>
  );
};
