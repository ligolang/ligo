import { faCopy, faDownload } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import React from 'react';
import { useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../redux/app';
import { ResultState } from '../redux/result';
import { Item, Toolbar } from './toolbar';
import { Tooltip } from './tooltip';

const Divider = styled.div`
  display: block;
  background-color: rgba(0, 0, 0, 0.12);
  height: 20px;
  width: 1px;
  margin: 0 3px;
`;

const Link = styled.a`
  font-size: 0.8em;
  color: var(--blue);
  opacity: 1;
`;

export const OutputToolbarComponent = (props: {
  onCopy?: () => void;
  onDownload?: () => void;
}) => {
  const output = useSelector<AppState, ResultState['output']>(
    state => state.result.output
  );

  return (
    <Toolbar>
      <Item onClick={() => props.onCopy && props.onCopy()}>
        <FontAwesomeIcon icon={faCopy}></FontAwesomeIcon>
        <Tooltip>Copy</Tooltip>
      </Item>
      <Item onClick={() => props.onDownload && props.onDownload()}>
        <FontAwesomeIcon icon={faDownload}></FontAwesomeIcon>
        <Tooltip>Download</Tooltip>
      </Item>
      <Divider></Divider>
      <Item>
        <Link
          target="_blank"
          rel="noopener noreferrer"
          href={`https://try-michelson.tzalpha.net/?source=${encodeURIComponent(
            output
          )}`}
        >
          View in Try-Michelson IDE
        </Link>
      </Item>
    </Toolbar>
  );
};
