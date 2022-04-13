import React from 'react'
import classnames from 'classnames'

import {
  FormGroup,
  Label,
  ButtonOptions,
  DropdownInput,
} from '@obsidians/ui-components'

import semver from 'semver'
import fileOps from '@obsidians/file-ops'
import notification from '@obsidians/notification'

import { NewProjectModal } from '@obsidians/workspace'

import FrameworkSelector from './FrameworkSelector'

const openZeppelinVersions = [
  { id: 'v4.2.0', display: 'v4.2.0' },
  { id: 'v4.1.0', display: 'v4.1.0' },
  { id: 'v4.0.0', display: 'v4.0.0' },
  { id: 'v3.4.1', display: 'v3.4.1' },
  { id: 'v3.3.0', display: 'v3.3.0' },
  { id: 'v3.2.0', display: 'v3.2.0' },
  { id: 'v3.1.0', display: 'v3.1.0' },
  { id: 'v3.0.2', display: 'v3.0.2' },
  { id: 'v2.5.1', display: 'v2.5.1' },
  { id: 'v2.4.0', display: 'v2.4.0' },
  { id: 'v2.3.0', display: 'v2.3.0' },
  { id: 'v2.2.0', display: 'v2.2.0' },
  // { id: 'v2.1.3', display: 'v2.1.3' },
  // { id: 'v2.0.0', display: 'v2.0.0' },
]

export default class ExtendedNewProjectModal extends NewProjectModal {
  constructor (props) {
    super(props)

    this.state = {
      ...this.state,
      framework: props.defaultFramework,
      npmClient: 'npm',
      openZeppelinVersion: 'v4.2.0',
    }

    this.framework = React.createRef()
  }

  componentDidUpdate () {
    const { group, framework } = this.state
    if (group === process.env.COMPILER_NAME && !framework.endsWith('-docker')) {
      this.setState({ framework: `${process.env.COMPILER_EXECUTABLE_NAME}-docker` })
    }
  }

  async createProject ({ projectRoot, name, template, group }) {
    if (this.props.createProject) {
      const createProject = this.props.createProject.bind(this)
      return createProject({ projectRoot, name, template, group })
    }

    const {
      remote,
      framework,
      npmClient,
      openZeppelinVersion,
    } = this.state

    if (remote) {
      return super.createProject({ projectRoot, name, template, compilerVersion })
    }

    const {
      name: compilerName,
      version: compilerVersion,
    } = this.framework.current.getNameAndVersion(framework, remote)

    if (!this.props.noCompilerOption && !compilerVersion) {
      notification.error('Cannot Create the Project', `Please select a version for ${compilerName}.`)
      return false
    }

    if (group === process.env.COMPILER_NAME) {
      this.setState({ showTerminal: true })
      if (!compilerVersion) {
        notification.error('Cannot Create the Project', `Please select a version for ${process.env.COMPILER_NAME}.`)
        return false
      }
      await fileOps.current.ensureDirectory(projectRoot)
      const projectDir = fileOps.current.getDockerMountPath(projectRoot)
      const cmd = [
        `docker run --rm -it`,
        `--name ${process.env.PROJECT}-create-project`,
        `-v "${projectDir}:/project/${name}"`,
        `-w "/project/${name}"`,
        `${process.env.DOCKER_IMAGE_COMPILER}:${compilerVersion}`,
        `${process.env.COMPILER_EXECUTABLE_NAME} unbox ${template}`,
      ].join(' ')

      const result = await this.terminal.current.exec(cmd)
      if (result.code) {
        notification.error('Cannot Create the Project')
        return false
      }

      const config = {
        main: './contracts/MetaCoin.sol',
        deploy: './build/contracts/MetaCoin.json',
        framework: `${process.env.COMPILER_EXECUTABLE_NAME}-docker`,
        npmClient,
        compilers: {
          [process.env.COMPILER_VERSION_KEY]: compilerVersion,
          solc: 'default'
        }
      }
      await fileOps.current.writeFile(fileOps.current.path.join(projectRoot, 'config.json'), JSON.stringify(config, null, 2))
      return { projectRoot, name }
    }

    let openZeppelinPackage
    if (group === 'open zeppelin') {
      openZeppelinPackage = `@openzeppelin/contracts`
      if (template === 'openzeppelin') {
        const hasERC1155 = semver.gte(openZeppelinVersion, 'v3.1.0')
        if (!hasERC1155) {
          template = 'openzeppelin-no-erc1155'
        }
        if (semver.lt(openZeppelinVersion, '3.0.0')) {
          openZeppelinPackage = 'openzeppelin-solidity'
          template = 'openzeppelin-v2'
        } else if (semver.gte(openZeppelinVersion, '4.0.0')) {
          template = 'openzeppelin-v4'
        }
      }
    }

    let result = await super.createProject({ projectRoot, name, template, framework, npmClient, compilerVersion, notify: false })
    if (!result) {
      return false
    }

    if (group === 'open zeppelin' || !framework.endsWith('-docker')) {
      this.setState({ showTerminal: true })
      const result = await this.terminal.current.exec(`${npmClient} init -y`, { cwd: projectRoot })
      if (result.code) {
        notification.error('Cannot Create the Project', 'Please make sure you have node.js installed.')
        return false
      }
    }

    const installCommand = npmClient === 'yarn' ? 'add --dev' : 'i -D'
    if (group === 'open zeppelin') {
      const result = await this.terminal.current.exec(`${npmClient} ${installCommand} ${openZeppelinPackage}@${openZeppelinVersion}`, { cwd: projectRoot })
      if (result.code) {
        notification.error('Fail to Install OpenZeppelin')
        return false
      }
    }

    if (!await this.framework.current.installDependencies({
      framework,
      npmClient,
      installCommand,
      compilerVersion,
      projectRoot,
      terminal: this.terminal.current,
    })) {
      return false
    }
    

    if (!framework.endsWith('-docker')) {
      result = await super.createProject({ projectRoot, name, template, framework }, 'post')
      if (!result) {
        return false
      }
    }
    return { projectRoot, name }
  }

  renderTemplate (renderSuper) {
    if (renderSuper) {
      return super.renderTemplate()
    }
    return null
  }

  renderOtherOptions = () => {
    const { FrameworkSelector } = this.props
    const { remote, group, openZeppelinVersion, framework, npmClient } = this.state
    if (this.props.noCompilerOption || remote) {
      return this.renderTemplate(true)
    }

    return (
      <>
        <div className='row'>
          <div className={classnames(group === 'open zeppelin' ? 'col-12 col-sm-8' : 'col-12')}>
            {this.renderTemplate(true)}
          </div>
          {
            group === 'open zeppelin' &&
            <div className='col-12 col-sm-4'>
              <DropdownInput
                label='Open Zeppelin Version'
                options={openZeppelinVersions}
                value={openZeppelinVersion}
                onChange={openZeppelinVersion => this.setState({ openZeppelinVersion })}
              />
          </div>
          }
        </div>
        <FrameworkSelector
          ref={this.framework}
          framework={framework}
          group={group}
          onSelectFramework={framework => this.setState({ framework })}
        />
        {
            (group === 'open zeppelin' || !framework.endsWith('-docker')) &&
            <FormGroup>
              <Label>Npm client</Label>
              <div>
                <ButtonOptions
                  size='sm'
                  className='mb-0'
                  options={[
                    { key: 'npm', text: 'npm' },
                    { key: 'yarn', text: 'yarn' },
                    { key: 'cnpm', text: 'cnpm' },
                  ]}
                  selected={npmClient}
                  onSelect={npmClient => this.setState({ npmClient })}
                />
              </div>
            </FormGroup>
          }
      </>
    )
  }
}

const templates = [
  { id: 'empty', display: 'Empty Project' },
  { id: 'coin', display: 'Coin' },
  { id: 'erc20', display: 'ERC20 Token' },
  {
    group: 'open zeppelin',
    badge: 'Open Zeppelin',
    local: true,
    children: [
      { id: 'openzeppelin', display: 'Basics - ERC20, ERC721 & ERC1155 (v3.1+)' },
    ],
  },
  {
    group: `${process.env.COMPILER_NAME}`,
    badge: `${process.env.COMPILER_NAME}`,
    local: true,
    children: [
      { id: 'metacoin', display: 'Metacoin' },
    ],
  }
]

ExtendedNewProjectModal.defaultProps = {
  defaultTemplate: 'coin',
  defaultFramework: 'truffle',
  templates,
  FrameworkSelector,
}
