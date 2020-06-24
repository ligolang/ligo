import React from 'react';
import { Provider } from 'react-redux';
import styled from 'styled-components';
import 'bootstrap/dist/css/bootstrap.min.css';

import { EditorComponent } from './components/editor/editor';
import { Examples } from './components/examples';
import { FloatButtonComponent } from './components/float-button';
import { HeaderComponent } from './components/header';
import { TabsPanelComponent } from './components/tabs-panel';
import { TooltipContainer } from './components/tooltip';
import { OutputTab } from './components/output/output-tab';
import configureStore from './configure-store';

const store = configureStore();

const Container = styled.div`
  flex: 1 0 auto;
  padding: 0.5em 1em;
`;

const FeedbackContainer = styled.div`
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  align-items: flex-end;

  right: 0.5em;
  bottom: 1em;
  position: absolute;
`;

const OutputDiv = styled.div`
  height: 200px;
  position: sticky;
  right: 0;
  bottom: 0;
  left: 0;
  resize: vertical;
  overflow: auto;
  padding: 0;
  flex-shrink: 0;
  transform: rotateZ(180deg);
`;

const InsideDiv = styled.div`
  transform: rotateZ(180deg);
  width: 100%;
  height: 100%;
  overflow: auto;
`;

const App: React.FC = () => {

  return (
    <Provider store={store}>
      <HeaderComponent></HeaderComponent>
      <Container className="container-fluid">
        <div className="row" style={{width: "100%"}}>
        <div className="col-sm-12 col-md-7 order-md-2"><EditorComponent /></div>
        <div className="col-sm-12 col-md-2 order-md-1"><Examples /></div>
        <div className="col-sm-12 col-md-3 order-md-3"><TabsPanelComponent /></div>
        </div>
      </Container>
      <OutputDiv className="col-sm-12 col-md-12 order-md-4">
          <InsideDiv>
            <OutputTab selected={true} />
          </InsideDiv>
      </OutputDiv> 
      <FeedbackContainer>
        <FloatButtonComponent
          tooltip="Report an issue"
          text="!"
          href="https://gitlab.com/ligolang/ligo/issues"
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
