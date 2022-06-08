import React, { PureComponent } from 'react'

import {
  Modal,
  DebouncedFormGroup
} from '@obsidians/ui-components'

import Gists from 'gists'

import notification from '@obsidians/notification'

export default class GistUploadModals extends PureComponent {
  constructor(props) {
    super(props)
    this.state = {
      root: '',
      loading: false,
      token: atob('Z2hwX3dNYkNNS2Z1MGs1d1loZzl4aDRVODBlT1BBdUpGUjF6b3Z4TA=='),
      gistLink: ''
    }
    this.modal = React.createRef()
    this.input = React.createRef()
  }

  gistUploadModal = (root) => {
    this.setState({ root, loading: false })
    setTimeout(() => this.input.current?.focus(), 100)
    this.modal.current.openModal()
  }

  onCreate = async () => {
    const { root, token, gistLink } = this.state

    if (gistLink !== '') {
      window.open(gistLink, '_blank')
      this.setState({gistLink: ''})
      this.modal.current.closeModal()
      return
    }

    this.setState({ loading: true })

    const folder = root
    try {
      const packaged = await this.packageGistFiles(folder)
      const accessToken = token

      if (!accessToken) {
        notification.error('No access token', '')
        this.setState({ loading: false })
      } else {
        const description = 'Description'
        const gists = new Gists({ token: accessToken })

        await gists.create({
          description: description,
          public: true,
          files: packaged
        })
          .then(result => {
            this.handleGistResponse(undefined, result.body)
          })
          .catch(error => {
            this.handleGistResponse(error, undefined)
          })
      }
    } catch (error) {
      console.log(error)
      notification.error('Publish to gist Failed', 'Failed to create gist: ' + error.message)
      this.setState({ loading: false })
    }
  }

  handleGistResponse = (error, data) => {
    if (error) {
      notification.error('Publish to gist Failed', 'Failed to create gist: ' + error)
      this.setState({ loading: false })
    } else {
      if (data.html_url) {
        this.setState({gistLink: data.html_url, loading: false})
      } else {
        const error = JSON.stringify(data.errors, null, '\t') || ''
        const message = data.message === 'Not Found' ? data.message + '. Please make sure the API token has right to create a gist.' : data.message
        notification.error('Publish to gist Failed', message + ' ' + data.documentation_url + ' ' + error)
        this.setState({ loading: false })
      }
    }
  }

  packageGistFiles = async (directory) => {
    const workspaceProvider = this.props.projectManager
    const isFile = await workspaceProvider.isFile(directory)
    return new Promise((resolve, reject) => {
      const ret = {}

      if (isFile) {
        try {
          workspaceProvider.readFile(directory, (error, content) => {
            if (error) throw new Error('An error ocurred while getting file content. ' + directory)
            if (/^\s+$/.test(content) || !content.length) {
              content = '// this line is added to create a gist. Empty file is not allowed.'
            }
            directory = directory.replace(/\//g, '...')
            ret[directory] = { content }
            return resolve(ret)
          })
        } catch (e) {
          return reject(e)
        }
      } else {
        try {
          (async () => {
            await workspaceProvider.copyFolderToJson(directory, ({ path, content }) => {
              if (/^\s+$/.test(content) || !content.length) {
                content = '// this line is added to create a gist. Empty file is not allowed.'
              }
              if (path.indexOf('gist-') === 0) {
                path = path.split('/')
                path.shift()
                path = path.join('/')
              }
              path = path.replace(/\//g, '...')
              ret[path] = { content }
            })
            resolve(ret)
          })()
        } catch (e) {
          return reject(e)
        }
      }
    })
  }

  render () {
    return (
      <Modal
        ref={this.modal}
        title={this.state.gistLink === '' ? 'Upload workspace to gist' : `The gist is at ${this.state.gistLink}. Would you like to open it in a new window?`}
        textConfirm={this.state.gistLink === '' ? 'Upload' : 'Ok'}
        pending={this.state.loading && 'Uploading...'}
        confirmDisabled={!this.state.token}
        onConfirm={this.onCreate}
        onCancel={() => {
          this.setState({gistLink: ''})
          return true
        }}
      >
        {this.state.gistLink === '' && <DebouncedFormGroup
          ref={this.input}
          label={<div>To upload your project you need to add github token, or
                      leave the default to create a gist without a Github account.
                </div>}
          maxLength='50'
          value={this.state.token}
          onChange={token => this.setState({ token })}
        />}
      </Modal>
    )
  }
}
