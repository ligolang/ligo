import { faCopy, faDownload } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import React from 'react';
import styled from 'styled-components';

import { Tooltip } from './tooltip';

const Container = styled.div`
  display: flex;
  justify-content: flex-start;
  padding: 0.2em 0.5em;
  z-index: 3;
`;

const Action = styled.div`
  z-index: 3;
  position: relative;
  margin: 4px 6px;
  cursor: pointer;

  opacity: 0.5;
  color: #444;

  ::before {
    content: '';
    display: block;
    position: absolute;
    z-index: -1;
    bottom: -4px;
    left: -4px;
    right: -4px;
    top: -4px;
    border-radius: 4px;
    background: none;
    box-sizing: border-box;
    opacity: 0;
    transform: scale(0);
    transition-property: transform, opacity;
    transition-duration: 0.15s;
    transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1);
  }

  :hover::before {
    background-color: rgba(32, 33, 36, 0.059);
    opacity: 1;
    transform: scale(1);
  }

  :hover {
    opacity: 1;
  }

  &:first-child {
    margin-left: 0;
  }

  &:last-child {
    margin-right: 0;
  }
`;

export const OutputToolbarComponent = (props: {
  onCopy?: () => void;
  onDownload?: () => void;
}) => {
  return (
    <Container>
      <Action onClick={() => props.onCopy && props.onCopy()}>
        <FontAwesomeIcon icon={faCopy}></FontAwesomeIcon>
        <Tooltip>Copy</Tooltip>
      </Action>
      <Action onClick={() => props.onDownload && props.onDownload()}>
        <FontAwesomeIcon icon={faDownload}></FontAwesomeIcon>
        <Tooltip>Download</Tooltip>
      </Action>
    </Container>
  );
};
