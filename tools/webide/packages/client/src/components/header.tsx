import React from 'react';
import styled from 'styled-components';

const Container = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;

  padding: 0.5em 1em;
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
  padding: 0em 1em;

  &:hover {
    color: #0e74ff;
  }
`;

export const HeaderComponent = () => {
  return (
    <Container>
      <Group>
        <a href="https://ligolang.org">
          <Logo src="/logo.svg" />
        </a>
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
