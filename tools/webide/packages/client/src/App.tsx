import React from 'react';
import { Provider } from 'react-redux';
import styled from 'styled-components';

import { EditorComponent } from './components/editor/editor';
import { Examples } from './components/examples';
import { FloatButtonComponent } from './components/float-button';
import { HeaderComponent } from './components/header';
import { TabsPanelComponent } from './components/tabs-panel';
import { TooltipContainer } from './components/tooltip';
import configureStore from './configure-store';

const store = configureStore();

const Container = styled.div`
  display: flex;
  padding: 0.5em 1em;
`;

const FeedbackContainer = styled.div`
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  align-items: flex-end;

  right: 1em;
  bottom: 1em;
  position: absolute;
`;

const App: React.FC = () => {
  return (
    <Provider store={store}>
      <HeaderComponent></HeaderComponent>
      <Container>
        <Examples></Examples>
        <EditorComponent></EditorComponent>
        <TabsPanelComponent></TabsPanelComponent>
      </Container>
      <FeedbackContainer>
        <FloatButtonComponent
          tooltip="Report an issue"
          text="!"
          href="https://gitlab.com/ligolang/ligo-web-ide/issues"
        ></FloatButtonComponent>
        <FloatButtonComponent
          tooltip="Ask a question"
          text="?"
          href="https://discord.gg/9rhYaEt"
        ></FloatButtonComponent>
      </FeedbackContainer>
      <TooltipContainer></TooltipContainer>
    </Provider>
  );
};

export default App;
