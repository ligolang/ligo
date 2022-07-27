import React, { PureComponent } from 'react'
import moment from 'moment'

import {
  Modal,
  DeleteButton,
} from '@obsidians/ui-components'

import DockerImageChannel from './DockerImageChannel'
import DownloadImageButton from './DownloadImageButton'

export default class DockerImageManager extends PureComponent {
  constructor (props) {
    super(props)

    this.state = {
      loading: false,
      installed: [],
    }

    this.modal = React.createRef()
  }

  get channel () {
    return this.props.channel || new DockerImageChannel(this.props.imageName)
  }

  openModal = () => {
    this.modal.current.openModal()
  }

  componentDidMount () {
    this.channel.onVersionsRefreshed(this.refreshVersions)
    this.fetchVersions()
  }

  componentDidUpdate (prevProps) {
    if (
      prevProps.channel !== this.props.channel ||
      prevProps.imageName !== this.props.imageName
    ) {
      this.fetchVersions()
    }
  }

  fetchVersions = () => {
    this.setState({ loading: true })
    this.channel.versions()
  }

  refreshVersions = versions => {
    this.setState({
      installed: versions,
      loading: false,
    })
    this.props.onRefresh(versions)
  }

  deleteVersion = async version => {
    this.setState({ loading: true })
    await this.channel.delete(version)
    await this.fetchVersions()
  }

  renderTableBody = () => {
    if (this.state.loading) {
      return (
        <tr key='loading'>
          <td align='middle' colSpan={4}>
            <i className='fas fa-spin fa-spinner mr-1' />Loading...
          </td>
        </tr>
      )
    }

    if (!this.state.installed.length) {
      const none = `(No ${this.props.noneName || this.props.imageName} installed)`
      return (
        <tr>
          <td align='middle' colSpan={4}>{none}</td>
        </tr>
      )
    }

    return (
      this.state.installed.map(v => (
        <tr key={`table-row-${v.Tag}`} className='hover-block'>
          <td>{v.Tag}</td>
          <td>{moment(v.CreatedAt, 'YYYY-MM-DD HH:mm:ss Z').format('LL')}</td>
          <td>{v.Size}</td>
          <td align='right'>
            <DeleteButton
              onConfirm={() => this.deleteVersion(v.Tag)}
              textConfirm='Click again to uninstall'
            />
          </td>
        </tr>
      ))
    )
  }

  render () {
    const imageName = this.channel.imageName
    const {
      modalTitle = `${imageName} Manager`,
      downloadingTitle = `Downloading ${imageName}`,
      extraFlags,
    } = this.props

    return (
      <Modal
        ref={this.modal}
        title={modalTitle}
        ActionBtn={
          <DownloadImageButton
            color='success'
            imageName={imageName}
            channel={this.channel}
            downloadingTitle={downloadingTitle}
            extraFlags={extraFlags}
            onDownloaded={this.fetchVersions}
          />
        }
      >
        <table className='table table-sm table-hover table-striped'>
          <thead>
            <tr>
              <th style={{ width: '40%' }}>version</th>
              <th style={{ width: '35%' }}>created</th>
              <th style={{ width: '15%' }}>size</th>
              <th style={{ width: '10%' }} />
            </tr>
          </thead>
          <tbody>
            {this.renderTableBody()}
          </tbody>
        </table>
      </Modal>
    )
  }
}
