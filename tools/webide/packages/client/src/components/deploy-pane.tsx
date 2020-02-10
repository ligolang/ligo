import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../redux/app';
import { ChangeEntrypointAction, ChangeStorageAction, DeployState, UseTezBridgeAction } from '../redux/deploy';
import { CheckboxComponent } from './checkbox';
import { Group, HGroup, Input, Label, Textarea } from './inputs';

const Container = styled.div``;

const Checkbox = styled(CheckboxComponent)`
  margin-right: 0.3em;
`;

const Hint = styled.span`
  font-style: italic;
  font-size: 0.8em;
`;

export const DeployPaneComponent = () => {
  const dispatch = useDispatch();
  const entrypoint = useSelector<AppState, DeployState['entrypoint']>(
    state => state.deploy.entrypoint
  );
  const storage = useSelector<AppState, DeployState['storage']>(
    state => state.deploy.storage
  );
  const useTezBridge = useSelector<AppState, DeployState['useTezBridge']>(
    state => state.deploy.useTezBridge
  );

  return (
    <Container>
      <Group>
        <Label htmlFor="entrypoint">Entrypoint</Label>
        <Input
          id="entrypoint"
          value={entrypoint}
          onChange={ev =>
            dispatch({ ...new ChangeEntrypointAction(ev.target.value) })
          }
        ></Input>
      </Group>
      <Group>
        <Label htmlFor="storage">Storage</Label>
        <Textarea
          id="storage"
          rows={9}
          value={storage}
          onChange={ev =>
            dispatch({ ...new ChangeStorageAction(ev.target.value) })
          }
        ></Textarea>
      </Group>
      <HGroup>
        <Checkbox
          checked={!useTezBridge}
          onChanged={value => dispatch({ ...new UseTezBridgeAction(!value) })}
        ></Checkbox>
        <Label htmlFor="tezbridge">
          We'll sign for you
          <br />
          <Hint>Got your own key? Deselect to sign with TezBridge</Hint>
        </Label>
      </HGroup>
    </Container>
  );
};
