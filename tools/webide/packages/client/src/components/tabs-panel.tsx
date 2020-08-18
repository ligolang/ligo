import React, { useState } from 'react';
import styled, { css } from 'styled-components';

import { ConfigureTabComponent } from './configure/configure-tab';

const Container = styled.div`
  flex: 1;
  display: flex;
  flex-direction: column;
`;

const Header = styled.div`
  display: flex;
  border-bottom: 5px solid var(--blue_trans1);
  min-height: 2.5em;
  margin-left: 15px;
`;

const Label = styled.span`
  cursor: pointer;
  user-select: none;
  flex: 1;
  display: flex;
  justify-content: center;
  align-items: center;

  &:hover {
    color: var(--orange);
  }
`;

const Tab = styled.div<{ selected?: boolean }>`
  flex: 1;
  display: flex;
  flex-direction: column;
`;

const Underline = styled.div<{ selectedTab: number }>`
  position: relative;
  top: -5px;
  background-color: var(--orange);
  height: 5px;
  margin-bottom: -5px;
  margin-left: 15px;
  width: 100%;
  transition: transform 0.2s ease-in;

  ${props =>
    css`
      transform: translateX(calc(${props.selectedTab} * 100%));
    `}
`;

const Content = styled.div`

  width: 100%;
  height: 100%;
  overflow: hidden;
`;

export const TabsPanelComponent = () => {
  const TABS = [
    { index: 0, label: 'Configure', id: 'configure-tab' }
  ];

  const [selectedTab, selectTab] = useState(TABS[0]);

  return (
    <Container>
      <Header>
        {TABS.map(tab => (
          <Tab
            key={tab.id}
            id={tab.id}
            selected={true}
          >
            <Label onClick={() => selectTab(tab)}>{tab.label}</Label>
          </Tab>
        ))}
      </Header>
      <Underline selectedTab={0}></Underline>
      <Content>
        <ConfigureTabComponent
          selected={true}
          onRun={() => {
            selectTab(TABS[1]);
          }}
        ></ConfigureTabComponent>
        {/* <OutputTab
          selected={selectedTab.index === 1}
          onCancel={() => {
            selectTab(TABS[0]);
          }}
        ></OutputTab> */}
      </Content>
    </Container>
  );
};
