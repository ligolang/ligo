import React, { FC, useEffect } from 'react';
import { connect } from 'react-redux';

import { ChangeSelectedAction } from '../redux/examples';
import { ExampleAction } from '../redux/actions/examples'

interface stateTypes {
  exampleId: any
}

interface dispatchTypes {
  setFile: (id) => any,
  getExample: (id) => any
}

const ViewExampleFile:FC<stateTypes&dispatchTypes> = (props) => {
  const { getExample, exampleId, setFile } = props

  useEffect(() => {
    if(exampleId) {
    getExample(exampleId).then((list) => {
      setFile(list)
    })
   }
  },[exampleId, getExample, setFile])

  return (
    <></>
  );
};

const mapStateToProps = state => {
  const { examples } = state
  return { 
    exampleId: examples && examples.list.length > 0 && examples.list[0].id
   }
}

const mapDispatchToProps = dispatch => {
  return({
    setFile: (id)  => dispatch({ ...new ChangeSelectedAction(id)}),
    getExample: (id)  => dispatch(ExampleAction(id))
  })
}

export default connect(mapStateToProps, mapDispatchToProps)(ViewExampleFile)