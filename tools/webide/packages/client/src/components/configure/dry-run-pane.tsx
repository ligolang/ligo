import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { protocolType } from '../../redux/compile';
import { ChangeEntrypointAction, ChangeParametersAction, ChangeProtocolAction, ChangeStorageAction, DryRunState } from '../../redux/dry-run';
import { AccessFunctionLabel, Group, Input, Label, Textarea } from '../form/inputs';
import { Option, SelectCommand } from '../form/select';

const Container = styled.div``;

export const DryRunPaneComponent = () => {
  const dispatch = useDispatch();
  const entrypoint = useSelector<AppState, DryRunState['entrypoint']>(
    state => state.dryRun && state.dryRun.entrypoint
  );
  const parameters = useSelector<AppState, DryRunState['parameters']>(
    state => state.dryRun && state.dryRun.parameters
  );
  const storage = useSelector<AppState, DryRunState['storage']>(
    state => state.dryRun && state.dryRun.storage
  );

  return (
    <Container>
      <Group>
        <Label htmlFor="protocol">Choose a protocol (used for compilation)</Label>
        <SelectCommand
          id="protocol-select"
          value={protocolType.Jakarta}
          onChange={ev =>
            dispatch({ ...new ChangeProtocolAction(ev.target.value) })
          }>
          <Option value={protocolType.Jakarta}>Jakarta</Option>
        </SelectCommand>
        <AccessFunctionLabel htmlFor="entrypoint"></AccessFunctionLabel>
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
          rows={5}
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
          rows={5}
          value={storage}
          onChange={ev =>
            dispatch({ ...new ChangeStorageAction(ev.target.value) })
          }
        ></Textarea>
      </Group>
    </Container>
  );
};
