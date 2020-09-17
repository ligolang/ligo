import React from 'react';
import { Provider } from 'react-redux';
import styled from 'styled-components';
import 'bootstrap/dist/css/bootstrap.min.css';
import SplitPane from 'react-split-pane';
import './index.css'

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


const InsideDiv = styled.div`
  width: 100%;
  height: 100%;
  overflow: auto;
`;

const SomeCustomHandle = {
  background: "rgb(255, 255, 255)",
  borderRadius: "2px",
  border: "1px solid rgb(221, 221, 221)",
  height: "100%",
  width: "100%",
  padding: 0
}

const App: React.FC = () => {

  return (
    <Provider store={store}>
      <SplitPane split="horizontal" defaultSize="75%" >
      <div style={{
          position: "absolute",
          height: "100%",
          right: "0",
          bottom: "0",
          left: "0"}}>
      <HeaderComponent></HeaderComponent>
      <Container className="container-fluid">
        <div className="row" style={{width: "100%"}}>
        <div className="col-sm-12 col-md-7 order-md-2"><EditorComponent /></div>
        <div className="col-sm-12 col-md-2 order-md-1"><Examples /></div>
        <div className="col-sm-12 col-md-3 order-md-3"><TabsPanelComponent /></div>
        </div>
      </Container>
      </div>
      <div style={{
          position: "relative",
          height: "100%",
          right: "0",
          bottom: "0",
          left: "0",
          overflow: "hidden",
          padding: "0"}}>
        <InsideDiv>
            <OutputTab selected={false} />
          </InsideDiv>
      </div>
      </SplitPane>
      <FeedbackContainer>
        <FloatButtonComponent
          tooltip="Report an issue"
          text="!"
          href="https://gitlab.com/ligolang/ligo/issues"
        ></FloatButtonComponent>
        <FloatButtonComponent
          tooltip="Ask a question"
          text="?"
          href="https://t.me/LigoLang"
        ></FloatButtonComponent>
      </FeedbackContainer>
      <TooltipContainer></TooltipContainer>
    </Provider>
  );
};

export default App;
