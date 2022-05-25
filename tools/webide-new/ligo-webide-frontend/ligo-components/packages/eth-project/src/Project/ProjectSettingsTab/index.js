import React from 'react'

import {
  DebouncedFormGroup,
  FormGroup,
  Label,
  CustomInput,
} from '@obsidians/ui-components'

import {
  WorkspaceContext,
  BaseProjectManager,
  AbstractProjectSettingsTab,
  ProjectPath,
} from '@obsidians/workspace'

import notification from '@obsidians/notification'
import { DockerImageInputSelector } from '@obsidians/docker'
import compilerManager from '@obsidians/eth-compiler'

import NewProjectModal from '../../components/NewProjectModal'

export default class ProjectSettingsTab extends AbstractProjectSettingsTab {
  static contextType = WorkspaceContext

  componentDidMount () {
    BaseProjectManager.channel.on('settings', this.debouncedUpdate)
  }
  
  componentWillUnmount () {
    BaseProjectManager.channel.off('settings', this.debouncedUpdate)
  }

  renderLanguageOption = projectSettings => {
    if (!this.props.languages?.length) {
      return null
    }

    return (
      <FormGroup>
        <Label>Project language</Label>
        <CustomInput
          id='settings-language'
          type='select'
          className='bg-black'
          value={projectSettings?.get('language')}
          onChange={event => this.onChange('language')(event.target.value)}
        >
          {this.props.languages.map(item => <option key={item.key} value={item.key}>{item.text}</option>)}
        </CustomInput>
      </FormGroup>
    )
  }

  render () {
    const { noSolc } = this.props
    const { projectRoot, projectManager, projectSettings } = this.context
    const framework = projectSettings?.get('framework')
    const readOnly = !projectManager.userOwnProject && projectManager.remote

    const frameworks = Object.entries(NewProjectModal.defaultProps.FrameworkSelector.frameworkNames)
      .map(([key, name]) => ({ key, name }))

    return (
      <div className='custom-tab bg2'>
        <div className='jumbotron bg-transparent text-body'>
          <div className='container'>
            <h1>Project Settings</h1>
            <form disabled={true}>
            <ProjectPath projectRoot={projectRoot} remote={projectManager.remote} />

            <h4 className='mt-4'>General</h4>
            {this.renderLanguageOption(projectSettings)}
            <DebouncedFormGroup
              label='Main file'
              className='bg-black'
              value={projectSettings?.get('main')}
              onChange={this.onChange('main')}
              placeholder={`Required`}
              readOnly={readOnly}
            />
            <DebouncedFormGroup
              label='Smart contract to deploy'
              className='bg-black'
              value={projectSettings?.get('deploy')}
              onChange={this.onChange('deploy')}
              placeholder={`Path to the built contract to deploy`}
              readOnly={readOnly}
            />
            {
              !projectManager.remote &&
              <FormGroup>
                <Label>Framework</Label>
                <CustomInput
                  id='settings-framework'
                  type='select'
                  className='bg-black'
                  value={framework}
                  onChange={event => {
                    notification.warning('Warning', 'Change framework is not recommended. The project may fail to compile and deploy unless you know how to set it up properly.')
                    this.onChange('framework')(event.target.value)
                  }}
                >
                  {frameworks.map(f => <option key={f.key} value={f.key}>{f.name}</option>)}
                </CustomInput>
              </FormGroup>
            }
            {
              !framework.endsWith('-docker') &&
              <FormGroup>
                <Label>Npm client</Label>
                <CustomInput
                  id='settings-npm-client'
                  type='select'
                  className='bg-black'
                  value={projectSettings?.get('npmClient')}
                  onChange={event => this.onChange('npmClient')(event.target.value)}
                >
                  <option value='npm'>npm</option>
                  <option value='yarn'>yarn</option>
                  <option value='cnpm'>cnpm</option>
                </CustomInput>
              </FormGroup>
            }
            <h4 className='mt-4'>Compilers</h4>
            {
              !projectManager.remote && framework === 'truffle' &&
              <DockerImageInputSelector
                channel={compilerManager.truffle}
                disableAutoSelection
                bg='bg-black'
                label={`${process.env.COMPILER_NAME_IN_LABEL} version`}
                noneName={`${process.env.COMPILER_NAME}`}
                modalTitle={`${process.env.COMPILER_NAME} Manager`}
                downloadingTitle={`Downloading ${process.env.COMPILER_NAME}`}
                selected={projectSettings?.get(`compilers.${process.env.COMPILER_VERSION_KEY}`)}
                onSelected={truffle => this.onChange(`compilers.${process.env.COMPILER_VERSION_KEY}`)(truffle)}
              />
            }
            {
              !noSolc &&
              <DockerImageInputSelector
                channel={compilerManager.solc}
                disableAutoSelection
                bg='bg-black'
                label='Solc version'
                noManager
                extraOptions={!projectManager.remote && framework === 'truffle' && [{
                  id: 'default',
                  display: 'From truffle-config.js',
                  onClick: () => this.onChange('compilers.solc')('default'),
                }]}
                selected={projectSettings?.get('compilers.solc')}
                onSelected={solc => this.onChange('compilers.solc')(solc)}
                readOnly={readOnly}
              />
            }
            <FormGroup>
              <Label>EVM version</Label>
              <CustomInput
                id='settings-evm-version'
                type='select'
                className='bg-black'
                value={projectSettings?.get('compilers.evmVersion')}
                onChange={event => this.onChange('compilers.evmVersion')(event.target.value)}
                disabled={!projectManager.userOwnProject}
              >
                <option value='berlin'>Berlin</option>
                <option value='istanbul'>Istanbul</option>
                <option value='petersburg'>Petersburg</option>
                <option value='constantinople'>Constantinople</option>
                <option value='byzantium'>Byzantium</option>
                <option value='spuriousDragon'>Spurious Dragon</option>
                <option value='tangerineWhistle'>Tangerine Whistle</option>
                <option value='homestead'>Homestead</option>
              </CustomInput>
            </FormGroup>
            <DebouncedFormGroup
              label='Optimizer runs'
              className='bg-black'
              placeholder='Default: 0 (disabled)'
              value={projectSettings?.get('compilers.optimizer.runs') || ''}
              readOnly={readOnly}
              onChange={value => {
                const runs = Number(value)
                if (runs) {
                  this.onChange('compilers.optimizer')({ enabled: true, runs })
                } else {
                  this.onChange('compilers.optimizer')({ enabled: false })
                }
              }}
            />

            <h4 className='mt-4'>Linter</h4>
            <FormGroup>
              <CustomInput
                id='settings-linter'
                type='select'
                className='bg-black'
                value={projectSettings?.get('linter')}
                onChange={event => this.onChange('linter')(event.target.value)}
                disabled={!projectManager.userOwnProject}
              >
                <option value='solhint'>Solhint</option>
                <option value='solium'>Solium/Ethlint</option>
              </CustomInput>
            </FormGroup>

            <h4 className='mt-4'>Editor</h4>
            <FormGroup>
              <Label>Font Family</Label>
              <CustomInput
                id='settings-font-family'
                type='select'
                className='bg-black'
                value={projectSettings?.get('editor.fontFamily')}
                onChange={event => this.onChange('editor.fontFamily')(event.target.value)}
                disabled={!projectManager.userOwnProject}
              >
                <option value='Hack'>Hack</option>
                <option value='Fira Code'>Fira Code</option>
              </CustomInput>
            </FormGroup>
            <FormGroup>
              <Label>Font Size</Label>
              <CustomInput
                id='settings-font-size'
                type='select'
                className='bg-black'
                value={projectSettings?.get('editor.fontSize')}
                onChange={event => this.onChange('editor.fontSize')(event.target.value)}
                disabled={!projectManager.userOwnProject}
              >
                <option value='11px'>11px</option>
                <option value='12px'>12px</option>
                <option value='13px'>13px</option>
                <option value='14px'>14px</option>
                <option value='15px'>15px</option>
                <option value='16px'>16px</option>
              </CustomInput>
            </FormGroup>
            <FormGroup>
              <Label>Font Ligatures</Label>
              <CustomInput
                id='settings-ligatures'
                type='select'
                className='bg-black'
                value={projectSettings?.get('editor.ligatures')}
                onChange={event => this.onChange('editor.ligatures')(event.target.value === 'true')}
                disabled={!projectManager.userOwnProject}
              >
                <option value='false'>Disabled</option>
                <option value='true'>Enabled</option>
              </CustomInput>
            </FormGroup>

            <AbstractProjectSettingsTab.DeleteButton context={this.context} />
            </form>
          </div>
        </div>
      </div>
    )
  }
}
