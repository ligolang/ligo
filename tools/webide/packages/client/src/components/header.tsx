import React from 'react';
import styled, { css } from 'styled-components';

const Container = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;

  padding: 0.3em 1em;
  font-family: 'DM Sans', 'Open Sans', sans-serif;

  box-shadow: 0px 1px 1px 0px rgba(0, 0, 0, 0.3);
`;

const Group = styled.div`
  display: flex;
  align-items: center;
`;

const Logo = styled.img`
  height: 32px;
`;

const Link = styled.a`
  text-decoration: none;
  color: black;
  padding: 0.5em 1em;

  &:hover {
    color: #0e74ff;
  }

  ${(props: { cheatSheetStyle?: boolean }) =>
    props.cheatSheetStyle &&
    css`
      background-color: #efefef;
      font-weight: 600;
      margin-left: 3em;

      &:hover {
        color: black;
      }
    `}
`;

export const HeaderComponent = () => {
  return (
    <Container>
      <Group>
        <a href="https://ligolang.org">
          <Logo src="/logo.svg" />
        </a>
        <Link cheatSheetStyle href="https://ligolang.org/docs/api/cheat-sheet" target="_blank">
          Cheat Sheet
        </Link>
      </Group>
      <Group>
        <Link href="https://ligolang.org/docs/intro/installation">Install</Link>
        <Link href="https://ligolang.org/docs/intro/introduction">Docs</Link>
        <Link href="https://ligolang.org/docs/tutorials/get-started/tezos-taco-shop-smart-contract">
          Tutorials
        </Link>
        <Link href="https://forum.tezosagora.org/tag/ligo" target="_blank">Blog</Link>
        <Link href="https://ligolang.org/contact">
          Ask Questions
        </Link>
      </Group>
    </Container>
  );
};
