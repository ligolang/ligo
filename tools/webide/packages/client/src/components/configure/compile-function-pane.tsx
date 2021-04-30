import React, {useEffect, useState} from 'react';
import { connect } from 'react-redux';
import styled from 'styled-components';
import { PushSpinner } from 'react-spinners-kit';
import { Group, Label } from '../form/inputs';
import { Option, Select } from '../form/select';
import { ListDeclarationAction } from '../../redux/actions/list-declaration'
import {ChangeSelectedAction} from '../../redux/compile-function'

const Container = styled.div``;

const SpinnerWrapper = styled.div`
  display: flex;
  justify-content: center;
  align-content: center;
  margin-top: 20%;
`;

const SelectCommand = styled(Select)`
  flex: 2;

  &:hover {
    box-shadow: var(--box-shadow);
  }
`;
export interface MethodType {
  declarations: string[];
}
const CompileFunctionPaneComponent = (props) => {

  const { getDeclarationList, code, setCompileFunction , language} = props
  const [declaration, setDeclaration] = useState<string[]>([])
  const [functionName, setFunctionName] = useState<string>(declaration[0])

  useEffect(() => {
    getDeclarationList(language, code).then((file: MethodType) => {
      setDeclaration(file.declarations)
    })
  }, [getDeclarationList, code, language]);

  return (
    <Container>
      {declaration.length <= 0 && 
        <SpinnerWrapper>
          <PushSpinner size={50} color="#fa6f41" />
        </SpinnerWrapper>
      }
      {declaration.length > 0 &&
      <Group>
        <Label>Select Function to compile</Label>
        <SelectCommand
          id="command-select"
          value={functionName}
          onChange={fn => {
            setFunctionName(fn)
            setCompileFunction(fn)
          }}
        >
        {declaration && declaration.map(m => {
          return(
            <Option key={m} value={m}>{m}</Option>
          )
        })}
        </SelectCommand>
      </Group>
}
    </Container>
  );
};

const mapStateToProps = state => {
  const { editor } = state
  return { 
    code : editor.code,
    language: editor.language
   }
}

const mapDispatchToProps = dispatch => {
  return({
    getDeclarationList: (syntax, code)  => dispatch(ListDeclarationAction(syntax, code)),
    setCompileFunction: (functionName)  => dispatch({...new ChangeSelectedAction(functionName)})
  })
}

export default connect(mapStateToProps, mapDispatchToProps)(CompileFunctionPaneComponent)