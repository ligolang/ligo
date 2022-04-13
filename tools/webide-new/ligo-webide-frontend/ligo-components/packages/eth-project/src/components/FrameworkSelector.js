import React, { PureComponent } from 'react'

import {
  FormGroup,
  Label,
  ButtonOptions,
  DropdownInput,
} from '@obsidians/ui-components'

import notification from '@obsidians/notification'
import { DockerImageInputSelector } from '@obsidians/docker'
import compilerManager from '@obsidians/compiler'

const frameworkNames = {
  truffle: 'Truffle',
  hardhat: 'Hardhat',
  waffle: 'Waffle',
  'truffle-docker': `Dockerized ${process.env.COMPILER_NAME}`,
}

const truffleVersions = [
  { id: 'v5.4.6', display: 'v5.4.6' },
  { id: 'v5.3.14', display: 'v5.3.13' },
  { id: 'v5.2.6', display: 'v5.2.6' },
  { id: 'v5.1.67', display: 'v5.1.67' },
  { id: 'v5.0.43', display: 'v5.0.43' },
  { id: 'v4.1.13', display: 'v4.1.13' },
]

const hardhatVersions = [
  { id: 'v2.5.0', display: 'v2.5.0' },
  { id: 'v2.4.3', display: 'v2.4.3' },
  { id: 'v2.3.3', display: 'v2.3.3' },
  { id: 'v2.2.1', display: 'v2.2.1' },
]

const waffleVersions = [
  { id: 'v3.4.0', display: 'v3.4.0' },
  { id: 'v3.3.0', display: 'v3.3.0' },
  { id: 'v3.2.2', display: 'v3.2.2' },
  { id: 'v3.1.2', display: 'v3.1.2' },
]

export default class FrameworkSelector extends PureComponent {
  static frameworkNames = frameworkNames

  constructor (props) {
    super(props)

    this.state = {
      truffleVersion: 'v5.4.6',
      hardhatVersion: 'v2.5.0',
      waffleVersion: 'v3.4.0',
      truffleDockerVersion: '',
    }
  }

  getNameAndVersion = (framework, remote) => {
    if (remote) {
      return { name: '', version: '' }
    }
    const name = frameworkNames[framework]
    const version = framework === 'truffle-docker'
      ? this.state.truffleDockerVersion
      : this.state[`${framework}Version`]
    return { name, version }
  }

  installDependencies = async ({
    framework,
    npmClient,
    installCommand,
    compilerVersion,
    projectRoot,
    terminal,
  }) => {
    if (framework === 'truffle') {
      const result = await terminal.exec(`${npmClient} ${installCommand} truffle@${compilerVersion}`, { cwd: projectRoot })
      if (result.code) {
        notification.error('Fail to Install Truffle')
        return false
      }
    } else if (framework === 'hardhat') {
      const result = await terminal.exec(`${npmClient} ${installCommand} hardhat@${compilerVersion} @nomiclabs/hardhat-waffle ethereum-waffle chai @nomiclabs/hardhat-ethers ethers`, { cwd: projectRoot })
      if (result.code) {
        notification.error('Fail to Install Hardhat')
        return false
      }
    } else if (framework === 'waffle') {
      const result = await terminal.exec(`${npmClient} ${installCommand} ethereum-waffle@${compilerVersion} ethers`, { cwd: projectRoot })
      if (result.code) {
        notification.error('Fail to Install Waffle')
        return false
      }
    }
    return true
  }

  renderFrameworkVersions = () => {
    const { framework } = this.props
    const { truffleVersion, hardhatVersion, waffleVersion, truffleDockerVersion } = this.state
    if (framework === 'truffle') {
      return (
        <FormGroup className='mb-2'>
          <Label>Truffle version</Label>
          <DropdownInput
            size='sm'
            label=''
            options={truffleVersions}
            value={truffleVersion}
            onChange={truffleVersion => this.setState({ truffleVersion })}
          />
        </FormGroup>
      )
    } else if (framework === 'hardhat') {
      return (
        <FormGroup className='mb-2'>
          <Label>Hardhat version</Label>
          <DropdownInput
            size='sm'
            label=''
            options={hardhatVersions}
            value={hardhatVersion}
            onChange={hardhatVersion => this.setState({ hardhatVersion })}
          />
        </FormGroup>
      )
    } else if (framework === 'waffle') {
      return (
        <FormGroup className='mb-2'>
          <Label>Waffle version</Label>
          <DropdownInput
            size='sm'
            label=''
            options={waffleVersions}
            value={waffleVersion}
            onChange={waffleVersion => this.setState({ waffleVersion })}
          />
        </FormGroup>
      )
    } else if (framework === 'truffle-docker') {
      return (
        <FormGroup className='mb-2'>
          <Label>{`${process.env.COMPILER_NAME_IN_LABEL} version`}</Label>
          <DockerImageInputSelector
            size='sm'
            key='truffle-selector'
            label=''
            channel={compilerManager.truffle}
            noneName={`${process.env.COMPILER_NAME}`}
            modalTitle={`${process.env.COMPILER_NAME} Manager`}
            downloadingTitle={`Downloading ${process.env.COMPILER_NAME}`}
            selected={truffleDockerVersion}
            onSelected={truffleDockerVersion => this.setState({ truffleDockerVersion })}
          />
        </FormGroup>
      )
    }
    return null
  }
 
  render () {
    const { framework, group, onSelectFramework } = this.props

    const options = [{ key: 'truffle-docker', text: frameworkNames['truffle-docker'] }]
    if (group !== process.env.COMPILER_NAME) {
      options.unshift({ key: 'waffle', text: frameworkNames.waffle })
      options.unshift({ key: 'hardhat', text: frameworkNames.hardhat })
      options.unshift({ key: 'truffle', text: frameworkNames.truffle })
    }

    return (
      <div className='row'>
        <div className='col-12 col-sm-7'>
          <FormGroup>
            <Label>Framework</Label>
            <div>
              <ButtonOptions
                size='sm'
                className='mb-0'
                options={options}
                selected={framework}
                onSelect={onSelectFramework}
              />
            </div>
          </FormGroup>
        </div>
        <div className='col-12 col-sm-5'>
          {this.renderFrameworkVersions()}
        </div>
      </div>
    )
  }
}
