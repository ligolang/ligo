import React from 'react'

import compilerManager from '../compilerManager'
import TruffleSelector from './TruffleSelector'
import SolcSelector from './SolcSelector'

export default props => {
  if (props.author === 'local') {
    return <>
      <TruffleSelector />
      <SolcSelector solc={compilerManager.solc} />
    </>
  }

  return <SolcSelector remote solc={compilerManager.solc} />
}
