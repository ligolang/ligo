import React from 'react'

import notification from '~/base-components/notification'
import keypairManager from '~/base-components/keypair'

import { withRouter } from 'react-router-dom'
import ReactJson from 'react-json-view'
import Highlight from 'react-highlight'

export default withRouter(({ format, actionResult, actionError, history, onNavigate }) => {
  const [_, forceUpdate] = React.useState({})

  React.useEffect(() => {
    return keypairManager.onUpdated(() => forceUpdate({}))
  }, [])

  if (actionError) {
    return <div><span>{actionError}</span></div>
  }

  if (actionResult) {
    if (format === 'pretty') {
      return (
        <ReactJson
          src={actionResult.parsed}
          theme='monokai'
          indentWidth={2}
          name={false}
          quotesOnKeys={false}
          displayArrayKey={false}
          enableClipboard={() => notification.info('Copied to Clipboard')}
          getLabel={addr => addr && keypairManager.getName(addr.toLowerCase())}
          onRedirect={link => {
            history.push(link)
            onNavigate && onNavigate()
          }}
        />
      )
    } else {
      return (
        <Highlight language='javascript' className='pre-wrap break-all small' element='pre'>
          <code>{JSON.stringify(actionResult.raw, null, 2)}</code>
        </Highlight>
      )
    }
  }

  return <div className='small'>(None)</div>
})
