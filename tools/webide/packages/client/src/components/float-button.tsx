import React, { useState } from 'react';
import styled, { css } from 'styled-components';

const Container = styled.div`
  display: flex;
  align-items: center;
  justify-content: center;
`;

const Button = styled.a`
  margin: 0.1em;
  width: 1.5em;
  height: 1.5em;
  border-radius: 50%;

  display: flex;
  justify-content: center;
  align-items: center;
  font-size: 1.5em;
  font-weight: bolder;
  text-decoration: none;

  color: rgba(255, 255, 255, 0.85);
  background-color: var(--button_float);
  box-shadow: 1px 3px 15px 0px rgba(153, 153, 153, 0.4);
  cursor: pointer;
  user-select: none;
  transform-origin: center center;
  transition: all 0.2s ease;

  &:hover {
    box-shadow: var(--box-shadow);
    background-color: var(--blue);
    color: rgb(255, 255, 255);
    transform: scale(1.2);
  }
`;

const Tooltip = styled.div<{ visible?: boolean }>`
  position: absolute;
  pointer-events: none;
  z-index: 3;
  white-space: nowrap;
  transform: translateX(-6.5em);

  font-size: var(--font_sub_size);
  color: var(--tooltip_foreground);
  background-color: var(--tooltip_background);
  border-radius: 6px;
  padding: 5px 10px;
  opacity: 0;
  transition: opacity 0.2s ease 0.2s;

  ${props =>
    props.visible &&
    css`
      opacity: 1;
    `}
`;

export const FloatButtonComponent = (props: {
  tooltip: string;
  text: string;
  href: string;
  className?: string;
}) => {
  const [isTooltipShowing, setShowTooltip] = useState(false);

  return (
    <Container className={props.className}>
      <Tooltip visible={isTooltipShowing}>{props.tooltip}</Tooltip>
      <Button
        onMouseOver={() => setShowTooltip(true)}
        onMouseOut={() => setShowTooltip(false)}
        href={props.href}
        target="_blank"
        rel="noopener noreferrer"
      >
        {props.text}
      </Button>
    </Container>
  );
};
