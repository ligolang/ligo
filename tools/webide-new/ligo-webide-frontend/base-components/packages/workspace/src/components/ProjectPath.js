import React from 'react'

import fileOps from '@obsidians/file-ops'
import notification from '@obsidians/notification'

export default function ProjectPath ({ projectRoot, remote }) {
  if (remote) {
    return (
      <kbd key='project-path-remote'>
        <span className='d-inline-block mr-1'>
          <i className='fas fa-cloud' />
        </span>
        {projectRoot}
      </kbd>
    )
  }

  const openProjectRoot = async () => {
    try {
      await fileOps.current.openItem(projectRoot)
    } catch (e) {
      notification.error('Failed', e.message)
    }
  }
  return (
    <kbd>
      <span
        key={`open-${projectRoot}`}
        className='d-inline-block hover-inline w-3 mr-1'
        onClick={openProjectRoot}
      >
        <i className='fas fa-folder-open hover-inline-show' />
        <i className='fas fa-folder hover-inline-hide' />
      </span>
      {projectRoot}
    </kbd>
  )
}
