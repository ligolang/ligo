import React, { PureComponent } from 'react'

import { Base64 } from 'js-base64'
import { Button } from '~/base-components/ui-components'
import platform from '~/base-components/platform'
import redux from '~/base-components/redux'

import Workspace from './components/Workspace'
import WorkspaceContext from './WorkspaceContext'

import ProjectLoading from './components/ProjectLoading'
import ProjectInvalid from './components/ProjectInvalid'

import actions from './actions'

export default class WorkspaceLoader extends PureComponent {
  constructor(props) {
    super(props)
    this.workspace = React.createRef()
    this.state = {
      loading: true,
      invalid: false,
      initial: null,
      terminal: false,
      context: {}
    }
  }

  componentDidMount() {
    this.prepareProject(this.props)
    if (this.props.addLanguages) {
      this.props.addLanguages()
    }
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.terminal !== prevState.terminal) {
      window.dispatchEvent(new Event('resize'))
    }
    if (this.props.projectRoot !== prevProps.projectRoot) {
      if (this.state.context.projectManager) {
        this.state.context.projectManager.dispose()
      }
      this.prepareProject(this.props)
    }
  }

  async prepareProject({ ProjectManager, projectRoot, type }) {
    if (projectRoot) {
      this.setState({ loading: true, invalid: false, context: {} })

      const projectManager = new ProjectManager[type](this, projectRoot)

      const result = await projectManager.prepareProject()
      if (result.error) {
        this.setState({ loading: false, invalid: true })
      } else {
        this.setState({
          loading: false,
          initial: result.initial,
          context: {
            projectRoot,
            projectManager,
            projectSettings: result.projectSettings,
          }
        })
        redux.dispatch('PROJECT_LOADED')
      }
    }
  }

  saveAll = async () => {
    return await this.workspace.current.saveAll()
  }

  toggleTerminal = terminal => {
    this.setState({ terminal })
    if (terminal) {
      this.props.compilerManager?.focus()
    }
  }

  openProjectSettings = settingsFilePath => {
    this.workspace.current.openFile({ path: settingsFilePath })
  }

  renderInvalidProject = projectRoot => {
    if (platform.isDesktop) {
      return (
        <ProjectInvalid projectRoot={projectRoot || '(undefined)'}>
          <Button
            color='secondary'
            onClick={() => this.removeProject(projectRoot)}
          >Remove</Button>
        </ProjectInvalid>
      )
    } else {
      return <ProjectInvalid projectRoot={projectRoot || '(undefined)'} />
    }
  }

  removeProject = projectRoot => {
    const id = Base64.encode(projectRoot)
    actions.removeProject({ id, name: projectRoot })
  }

  render() {
    const {
      projectRoot,
      ProjectToolbar,
      signer,
      CompilerTerminal,
    } = this.props
    const { loading, invalid, initial, terminal, context } = this.state

    if (loading) {
      return <ProjectLoading projectRoot={projectRoot} />
    }

    if (invalid) {
      return this.renderInvalidProject(projectRoot)
    }

    return (
      <WorkspaceContext.Provider value={context}>
        <Workspace
          ref={this.workspace}
          theme={this.props.theme}
          initial={initial}
          terminal={terminal}
          defaultSize={272}
          makeContextMenu={this.props.makeContextMenu}
          ProjectToolbar={ProjectToolbar}
          signer={signer}
          Terminal={
            CompilerTerminal &&
            <CompilerTerminal projectManager={context.projectManager} active={terminal} cwd={projectRoot} />
          }
        />
      </WorkspaceContext.Provider>
    )
  }
}
