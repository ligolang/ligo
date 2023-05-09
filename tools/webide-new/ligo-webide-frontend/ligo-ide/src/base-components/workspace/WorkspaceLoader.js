import React, { PureComponent } from "react";

import { Base64 } from "js-base64";
import { Button } from "~/base-components/ui-components";
import redux from "~/base-components/redux";

import Workspace from "./components/Workspace";
import WorkspaceContext from "./WorkspaceContext";

import ProjectLoading from "./components/ProjectLoading";
import ProjectInvalid from "./components/ProjectInvalid";
import ProjectManager from "./ProjectManager/ProjectManager";
import { CompilerManager } from "~/ligo-components/eth-compiler";

import actions from "./actions";

export class WorkspaceLoader extends PureComponent {
  constructor(props) {
    super(props);
    this.workspace = React.createRef();
    this.state = {
      loading: true,
      invalid: false,
      initial: null,
      terminal: false,
      context: {},
    };
  }

  componentDidMount() {
    this.prepareProject(this.props);
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.terminal !== prevState.terminal) {
      window.dispatchEvent(new Event("resize"));
    }
    if (this.props.projectRoot !== prevProps.projectRoot) {
      this.prepareProject(this.props);
    }
  }

  async prepareProject({ projectRoot, type }) {
    if (projectRoot) {
      this.setState({ loading: true, invalid: false, context: {} });

      const projectManager = new ProjectManager(this, projectRoot);

      const result = await projectManager.prepareProject();
      if (result.error) {
        this.setState({ loading: false, invalid: true });
      } else {
        this.setState({
          loading: false,
          initial: result.initial,
          context: {
            projectRoot,
            projectManager,
            projectSettings: result.projectSettings,
          },
        });
        redux.dispatch("PROJECT_LOADED");
      }
    }
  }

  saveAll = async () => {
    return await this.workspace.current.saveAll();
  };

  toggleTerminal = (terminal) => {
    this.setState({ terminal });
    if (terminal) {
      CompilerManager.focus();
    }
  };

  openProjectSettings = (settingsFilePath) => {
    this.workspace.current.openFile({ path: settingsFilePath });
  };

  renderInvalidProject = (projectRoot) => {
    return <ProjectInvalid projectRoot={projectRoot || "(undefined)"} />;
  };

  removeProject = (projectRoot) => {
    const id = Base64.encode(projectRoot);
    actions.removeProject({ id, name: projectRoot });
  };

  render() {
    const { projectRoot, ProjectToolbar, signer, CompilerTerminal } = this.props;
    const { loading, invalid, initial, terminal, context } = this.state;

    actions.projectManager = context.projectManager;

    if (loading) {
      return <ProjectLoading projectRoot={projectRoot} />;
    }

    if (invalid) {
      return this.renderInvalidProject(projectRoot);
    }

    return (
      <WorkspaceContext.Provider value={context}>
        <Workspace
          ref={this.workspace}
          addLanguagesCallback={this.props.addLanguages}
          theme={this.props.theme}
          initial={initial}
          terminal={terminal}
          defaultSize={272}
          makeContextMenu={this.props.makeContextMenu}
          ProjectToolbar={ProjectToolbar}
          signer={signer}
          Terminal={
            CompilerTerminal && (
              <CompilerTerminal
                projectManager={context.projectManager}
                active={terminal}
                cwd={projectRoot}
              />
            )
          }
        />
      </WorkspaceContext.Provider>
    );
  }
}
