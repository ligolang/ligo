import { faCopy } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import React, { useEffect, useRef, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { Dispatch } from 'redux';
import styled, { css } from 'styled-components';

import { AppState } from '../redux/app';
import { ChangeShareLinkAction, ShareState } from '../redux/share';
import { share } from '../services/api';
import { Tooltip } from './tooltip';

const Container = styled.div`
  display: flex;
  justify-content: flex-end;
  align-items: center;
`;

const Button = styled.div<{ clicked?: boolean }>`
  cursor: pointer;
  user-select: none;

  z-index: 1;
  display: flex;
  justify-content: center;
  align-items: center;

  height: 2em;
  width: 6em;
  color: var(--blue);
  background-color: white;
  border-radius: 1em;
  transition: width 0.3s ease-in;

  ${props =>
    props.clicked &&
    css`
      width: 2em;
      background-color: white;
    `}

  &:hover {
    background-color: var(--blue_opaque1);
  }
`;

const Label = styled.span<{ visible?: boolean }>`
  pointer-events: none;
  opacity: 1;
  transition: opacity 0.3s ease-in;

  ${props =>
    !props.visible &&
    css`
      opacity: 0;
    `}
`;

const CopyIcon = ({ visible, ...props }: { visible: boolean }) => (
  <FontAwesomeIcon {...props} icon={faCopy}></FontAwesomeIcon>
);

const Copy = styled(CopyIcon)`
  position: absolute;
  pointer-events: none;
  opacity: 1;
  transition: opacity 0.3s ease-in;

  ${props =>
    !props.visible &&
    css`
      opacity: 0;
    `}
`;

const Input = styled.input<{ visible?: boolean }>`
  position: absolute;
  background-color: var(--blue);
  border-radius: 1em;

  opacity: 0;
  height: 2em;
  width: 2em;
  transform: translateX(-0.3em);
  border: none;
  padding: 0 1em;
  font-size: 1em;
  color: white;

  transition: width 0.3s ease-in;
  outline: none;
  ${props =>
    props.visible &&
    css`
      opacity: 1;
      width: 25em;
    `}
`;

const shareAction = () => {
  return async function(dispatch: Dispatch, getState: () => AppState) {
    try {
      const { hash } = await share(getState());
      dispatch({ ...new ChangeShareLinkAction(hash) });
    } catch (ex) {}
  };
};

function copy(element: HTMLInputElement): boolean {
  element.select();
  element.setSelectionRange(0, 99999);
  return document.execCommand('copy');
}

export const ShareComponent = () => {
  const inputEl = useRef<HTMLInputElement>(null);
  const dispatch = useDispatch();
  const shareLink = useSelector<AppState, ShareState['link']>(
    state => state.share.link
  );
  const [clicked, setClicked] = useState(false);

  const SHARE_TOOLTIP = 'Share code';
  const COPY_TOOLTIP = 'Copy link';
  const COPIED_TOOLTIP = 'Copied!';
  const [tooltipMessage, setTooltipMessage] = useState(SHARE_TOOLTIP);

  useEffect(() => {
    if (shareLink) {
      if (inputEl.current && copy(inputEl.current)) {
        setTooltipMessage(COPIED_TOOLTIP);
      } else {
        setClicked(true);
        setTooltipMessage(COPY_TOOLTIP);
      }
    } else {
      setClicked(false);
      setTooltipMessage(SHARE_TOOLTIP);
    }
  }, [shareLink]);

  return (
    <Container>
      <Input
        id="share-link"
        visible={!!shareLink}
        readOnly
        ref={inputEl}
        value={shareLink ? `${window.location.origin}/p/${shareLink}` : ''}
      ></Input>
      <Button
        id="share"
        clicked={clicked}
        onMouseOver={() => {
          if (tooltipMessage === COPIED_TOOLTIP) {
            setTooltipMessage(COPY_TOOLTIP);
          }
        }}
        onClick={() => {
          if (!shareLink) {
            dispatch(shareAction());
            setClicked(true);
            setTooltipMessage(COPY_TOOLTIP);
          } else if (inputEl.current) {
            copy(inputEl.current);
            setTooltipMessage(COPIED_TOOLTIP);
          }
        }}
      >
        <Label visible={!clicked}>Share</Label>
        <Copy visible={clicked}></Copy>
        <Tooltip>{tooltipMessage}</Tooltip>
      </Button>
    </Container>
  );
};
