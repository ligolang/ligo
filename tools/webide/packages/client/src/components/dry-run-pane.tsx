import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../redux/app';
import { ChangeEntrypointAction, ChangeParametersAction, ChangeStorageAction, DryRunState } from '../redux/dry-run';
import { Group, Input, Label, Textarea } from './inputs';

const Container = styled.div``;

export const DryRunPaneComponent = () => {
  const dispatch = useDispatch();
  const entrypoint = useSelector<AppState, DryRunState['entrypoint']>(
    state => state.dryRun.entrypoint
  );
  const parameters = useSelector<AppState, DryRunState['parameters']>(
    state => state.dryRun.parameters
  );
  const storage = useSelector<AppState, DryRunState['storage']>(
    state => state.dryRun.storage
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
        <Label htmlFor="parameters">Parameters</Label>
        <Textarea
          id="parameters"
          rows={9}
          value={parameters}
          onChange={ev =>
            dispatch({ ...new ChangeParametersAction(ev.target.value) })
          }
        ></Textarea>
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
    </Container>
  );
};
