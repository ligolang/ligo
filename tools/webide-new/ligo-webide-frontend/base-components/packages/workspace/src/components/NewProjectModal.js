import React, { PureComponent } from 'react'

import {
  Modal,
  ButtonOptions,
  FormGroup,
  Label,
  InputGroup,
  InputGroupAddon,
  Input,
  Button,
  DebouncedFormGroup,
  DropdownInput,
} from '@obsidians/ui-components'

import platform from '@obsidians/platform'
import fileOps from '@obsidians/file-ops'
import Auth from '@obsidians/auth'
import notification from '@obsidians/notification'
import Terminal from '@obsidians/terminal'

import ProjectManager from '../ProjectManager'
import actions from '../actions'

export default class NewProjectModal extends PureComponent {
  constructor (props) {
    super(props)

    this.state = {
      remote: platform.isWeb,
      name: '',
      invalid: false,
      projectRoot: '',
      template: props.defaultTemplate,
      group: props.defaultGroup,
      creating: false,
      showTerminal: false,
    }

    this.modal = React.createRef()
    this.terminal = React.createRef()
    this.path = fileOps.current.path
    this.fs = fileOps.current.fs

    actions.newProjectModal = this
  }

  openModal (remote) {
    const { defaultTemplate, defaultGroup } = this.props
    this.setState({
      remote,
      template: defaultTemplate,
      group: defaultGroup,
      creating: false,
      showTerminal: false,
    })
    this.forceUpdate()
    this.modal.current.openModal()
    return new Promise(resolve => { this.onConfirm = resolve })
  }

  chooseProjectPath = async () => {
    try {
      const projectRoot = await fileOps.current.chooseFolder()
      this.setState({ projectRoot })
    } catch (e) {

    }
  }

  onCreateProject = async () => {
    this.setState({ creating: true })

    const { remote, name, template, group } = this.state

    let projectRoot
    if (!remote) {
      if (!this.state.projectRoot) {
        projectRoot = this.path.join(fileOps.current.workspace, name)
      } else if (!this.path.isAbsolute(this.state.projectRoot)) {
        projectRoot = this.path.join(fileOps.current.workspace, this.state.projectRoot)
      } else {
        projectRoot = this.state.projectRoot
      }
    }

    const created = await this.createProject({ projectRoot, name, template, group })

    if (created) {
      this.modal.current.closeModal()
      this.onConfirm(created)
      this.setState({ name: '', projectRoot: '', template: this.props.defaultTemplate, creating: false, showTerminal: false })
    } else {
      this.setState({ creating: false })
    }
  }

  async createProject ({ notify = true, ...options }, stage = '') {
    try {
      const Manager = this.state.remote ? ProjectManager.Remote : ProjectManager.Local
      const created = await Manager.createProject(options, stage)
      if (notify) {
        notification.success('Successful', `New project <b>${options.name}</b> is created.`)
      }
      return created
    } catch (e) {
      // if (notify) {
      notification.error('Cannot Create the Project', e.message)
      // }
      return false
    }
  }

  renderLocation = () => {
    if (platform.isDesktop && Auth.username) {
      return (
        <div>
          <ButtonOptions
            className='mb-3'
            options={[
              { key: 'local', text: 'Local', icon: 'far fa-desktop mr-1' },
              { key: 'cloud', text: 'Cloud', icon: 'far fa-cloud mr-1' },
            ]}
            selected={this.state.remote ? 'cloud' : 'local'}
            onSelect={key => this.setState({ remote: key === 'cloud' })}
          />
        </div>
      )
    }
    return null
  }

  renderProjectPath = () => {
    if (this.state.remote) {
      return null
    }

    let placeholder = 'Project path'
    if (!this.state.projectRoot) {
      placeholder = this.path.join(fileOps.current.workspace, this.state.name || '')
    }

    return (
      <FormGroup>
        <Label>Project location</Label>
        <InputGroup>
          <Input
            placeholder={placeholder}
            value={this.state.projectRoot}
            onChange={e => this.setState({ projectRoot: e.target.value })}
          />
          <InputGroupAddon addonType='append'>
            <Button color='secondary' onClick={this.chooseProjectPath}>
              Choose...
            </Button>
          </InputGroupAddon>
        </InputGroup>
      </FormGroup>
    )
  }

  renderTemplate () {
    const { noTemplate, templates } = this.props
    const { remote, template } = this.state
    if (noTemplate) {
      return null
    }
    return (
      <DropdownInput
        label='Template'
        options={templates.filter(t => !remote || !t.local)}
        placeholder='(Please select a template)'
        value={template}
        onChange={(template, group) => this.setState({ template, group })}
      />
    )
  }

  renderOtherOptions = () => null

  render () {
    const { projectNameProps, templates } = this.props
    const { name, invalid, creating, showTerminal } = this.state

    return (
      <Modal
        ref={this.modal}
        title='Create a New Project'
        textConfirm='Create Project'
        onConfirm={this.onCreateProject}
        pending={creating && 'Creating...'}
        confirmDisabled={!name || invalid}
      >
        {this.renderLocation()}
        {this.renderProjectPath()}
        <DebouncedFormGroup
          label='Project name'
          value={name}
          onChange={(name, invalid) => this.setState({ name, invalid })}
          {...projectNameProps}
        />
        {this.renderTemplate()}
        {this.renderOtherOptions()}
        <div style={{ display: showTerminal ? 'block' : 'none'}}>
          <Terminal
            ref={this.terminal}
            active={showTerminal}
            height='200px'
            logId='create-project'
            className='rounded overflow-hidden'
          />
        </div>
      </Modal>
    )
  }
}
