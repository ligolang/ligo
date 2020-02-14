import React from 'react';
import styled from 'styled-components';

import { MonacoComponent } from './monaco';
import { ShareComponent } from './share';
import { SyntaxSelectComponent } from './syntax-select';

const Container = styled.div`
  flex: 2;
`;

const Header = styled.div`
  flex: 1;
  display: flex;
  justify-content: space-between;
  align-items: center;

  min-height: 2.5em;
  border-bottom: 5px solid var(--blue_trans1);
`;

export const EditorComponent = () => {
  return (
    <Container>
      <Header>
        <SyntaxSelectComponent></SyntaxSelectComponent>
        <ShareComponent></ShareComponent>
      </Header>
      <MonacoComponent></MonacoComponent>
    </Container>
  );
};
