import React, { PureComponent } from 'react'
import {
  Button,
  ListGroup,
  ListGroupItem
} from '@obsidians/ui-components'

import fileOps from '@obsidians/file-ops'
import { ListItemDocker, ListItemDockerImage } from '@obsidians/docker'

import { instanceChannel } from '@obsidians/eth-network'
import compiler from '@obsidians/eth-compiler'
import { t } from '@obsidians/i18n'
import platform from '@obsidians/platform'
import checkDependencies from './checkDependencies'
import PropTypes from 'prop-types'

const {
  PROJECT_GITHUB_REPO
} = process.env
const tutorialPanelInfo = {
  description: `Black IDE is a graphic IDE for developing smart contracts on the Ethereum blockchian. New here ? Don't worry.
Here is an instruction for a quick scan and details of each features.`,
  tips: 'To get started, please install the prerequisite tools for Black IDE',
  nextPage: `${PROJECT_GITHUB_REPO}/blob/master/README.md`
}

export default class Welcome extends PureComponent {
  static defaultProps = {
    nodeSubtitle: `${process.env.CHAIN_NAME} node built into a docker image.`,
    truffleSubtitle: `${process.env.CHAIN_NAME} version of truffle used to create and compile a project.`,
    enableTutorial: false
  }

  constructor (props) {
    super(props)
    this.state = {
      ready: false,
      remote: platform.isWeb
    }
    this.tutorialBar = this.tutorialBar.bind(this)
    this.toGuidePage = this.toGuidePage.bind(this)

    this.listItemDocker = React.createRef()
    this.imageRefs = new Array(2 + (props.extraItems?.length || 0))
      .fill(null)
      .map(() => React.createRef())
  }

  componentDidMount () {
    this.mounted = true
    this.refresh()
    fileOps.current.onFocus(this.refresh)
  }

  componentWillUnmount () {
    this.mounted = false
    fileOps.current.offFocus(this.refresh)
  }

  getImageItems = (props = this.props) => {
    const { extraItems = [] } = props
    return [
      {
        channel: instanceChannel.node,
        title: `${process.env.CHAIN_EXECUTABLE_NAME} in Docker`,
        subtitle: props.nodeSubtitle,
        link: `https://hub.docker.com/r/${process.env.DOCKER_IMAGE_NODE}`,
        downloadingTitle: `Downloading ${process.env.CHAIN_EXECUTABLE_NAME}`,
      },
      {
        channel: compiler.truffle,
        title: `${process.env.COMPILER_NAME} in Docker`,
        subtitle: props.truffleSubtitle,
        link: `https://hub.docker.com/r/${process.env.DOCKER_IMAGE_COMPILER}`,
        downloadingTitle: `Downloading ${process.env.COMPILER_NAME}`,
      },
      ...extraItems,
    ]
  }

  refresh = async () => {
    if (this.mounted) {
      this.listItemDocker.current.refresh()
      this.imageRefs.forEach(ref => ref.current?.refresh())
      const ready = await checkDependencies(this.props.extraItems)
      this.setState({ ready })
    }
  }

  toGuidePage() {
    fileOps.current.openLink(tutorialPanelInfo.nextPage)
  }

  tutorialBar () {
    return (
      this.state.remote ? null :
        <div>
          <p>{tutorialPanelInfo.description }</p>
          <ListGroupItem className='center' style={{
            'margin': '10px 0',
            'borderRadius': '6px'
          }}>
            <div className='center'>
              <div className='tutorialPanel' />
              <p>Learn how to use Black IDE</p>
            </div>

            <Button
              onClick={this.toGuidePage}
              color={'primary'}>
              Open
            </Button>
          </ListGroupItem>

          <p>{ tutorialPanelInfo.tips }</p>
        </div>
    )
  }

  render () {
    return (
      <div className='d-flex h-100 overflow-auto'>
        <div className='jumbotron jumbotron-fluid'>
          <div className='container'>
            <h4 className='display-4'>{t('welcome.welcome', { projectName: process.env.PROJECT_NAME })}</h4>
            { this.props.enableTutorial ? this.tutorialBar() : null }
            <div className='my-3' />

            <ListGroup>
              <ListItemDocker
                ref={this.listItemDocker}
                onStartedDocker={this.refresh}
              />
              {this.getImageItems().map((item, i) => (
                <ListItemDockerImage
                  key={`docker-image-${i}`}
                  ref={this.imageRefs[i]}
                  channel={item.channel}
                  title={item.title}
                  subtitle={item.subtitle}
                  link={item.link}
                  downloadingTitle={item.downloadingTitle}
                  onInstalled={this.refresh}
                />
              ))}
            </ListGroup>
            <Button
              block
              color={this.state.ready ? 'primary' : 'secondary'}
              size='lg'
              className='my-5 mx-auto'
              style={{ width: 'fit-content' }}
              onClick={this.props.onGetStarted}
            >
              {this.state.ready ? t('welcome.start') : t('welcome.skip')}
            </Button>
          </div>
        </div>
      </div>
    )
  }
}

Welcome.propTypes = {
  enableTutorial: PropTypes.boolean
}
