import React from 'react';
import styled, { css } from 'styled-components';

const Container = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;

  padding: 0.5em 1em;
  font-family: 'DM Sans', 'Open Sans', sans-serif;
  box-shadow: 0px 2px 3px 0px rgba(0, 0, 0, 0.3);
`;

const Group = styled.div`
  display: flex;
  align-items: center;
`;

const Logo = styled.div`
  font-size: 1.25em;
`;

const Link = styled.a`
  text-decoration: none;
  color: black;
  padding: 0.5em 1em;

  &:hover {
    color: #3aa0ff;
  }

  ${(props: { versionStyle?: boolean }) =>
    props.versionStyle &&
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
        <Link href="https://ligolang.org">
          <Logo>LIGO</Logo>
        </Link>
        <Link versionStyle href="https://ligolang.org/versions">
          next
        </Link>
      </Group>
      <Group>
        <Link href="https://ligolang.org/docs/intro/installation">Docs</Link>
        <Link href="https://ligolang.org/docs/tutorials/get-started/tezos-taco-shop-smart-contract">
          Tutorials
        </Link>
        <Link href="https://ligolang.org/blog">Blog</Link>
        <Link href="https://ligolang.org/docs/contributors/origin">
          Contribute
        </Link>
      </Group>
    </Container>
  );
};
