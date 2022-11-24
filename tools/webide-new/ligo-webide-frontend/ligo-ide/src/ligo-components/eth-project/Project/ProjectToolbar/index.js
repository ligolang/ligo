import React, { PureComponent } from "react";

import { WorkspaceContext } from "~/base-components/workspace";
import { ToolbarButton, DropdownToolbarButton } from "~/base-components/ui-components";
import keypairManager from "~/base-components/keypair";
import DeployScriptModal from "./DeployScriptModal";
import CompileModal from "./CompileModal";
import ExpressionManagerModal from "./ExpressionManagerModal";

// import DeployButton from './DeployButton'
import SignRequestModal from "./SignRequestModal";

export default class ProjectToolbar extends PureComponent {
  // eslint-disable-next-line react/static-property-placement
  static contextType = WorkspaceContext;

  constructor(props) {
    super(props);
    this.deployScriptModalRef = React.createRef();
    this.deployModalRef = React.createRef();
    this.expressionManagerModal = React.createRef();
    this.compileModalRef = React.createRef();
    this.state = {
      isExpressionManagerModalOpen: false,
      currentTab: "",
      expressionManagerType: "",
      tzFilePath: "",
    };
  }

  deployScriptModal = () => {
    this.deployScriptModalRef.current.openModal();
  };

  deployModal = () => {
    this.deployModalRef.current.openModal();
  };

  compileModalOpen = () => {
    const tzFilePath = this.context.projectSettings?.get("main") || "";
    this.setState({ tzFilePath });
    this.compileModalRef.current.openModal();
  };

  expressionExecutionModal = (type) => {
    this.setState({
      currentTab: this.props.editor.current.tabs.current.currentTab.path,
      isExpressionManagerModalOpen: true,
      expressionManagerType: type,
    });
  };

  render() {
    const { signer, noBuild, noDeploy, ExtraButtons = () => null } = this.props;
    const { projectSettings, projectManager } = this.context;
    const compilers = projectSettings?.get("compilers") || {};
    const readOnly = !projectManager.userOwnProject && projectManager.remote;

    return (
      <>
        <ToolbarButton
          id="compile"
          icon="fas fa-hammer"
          tooltip="Compile"
          readOnly={readOnly}
          onClick={() => this.compileModalOpen()}
        />
        <ToolbarButton
          id="deploy-script"
          icon="fas fa-file-export"
          tooltip="Deploy Script"
          readOnly={readOnly}
          onClick={() => this.deployScriptModal()}
        />
        <ToolbarButton
          id="dry-run"
          icon="fas fa-sun"
          tooltip="Dry Run"
          readOnly={readOnly}
          onClick={() => this.expressionExecutionModal("dryRun")}
        />
        <ToolbarButton
          id="compile-expr"
          icon="fas fa-wrench"
          tooltip="Compile Expression"
          readOnly={readOnly}
          onClick={() => this.expressionExecutionModal("compile")}
        />
        {/* { !noDeploy && <DeployButton projectManager={projectManager} signer={signer} /> } */}
        <ExtraButtons projectManager={projectManager} signer={signer} />
        <div className="flex-1" />
        <ToolbarButton
          id="settings"
          icon="fas fa-cog"
          tooltip="Project Settings"
          onClick={() => projectManager.openProjectSettings()}
        />
        <SignRequestModal ref={keypairManager.signReqModal} />
        <CompileModal
          modalRef={this.compileModalRef}
          tzFilePath={this.state.tzFilePath}
          onCompile={() => projectManager.compile(null, this.props.finalCall)}
        />
        <DeployScriptModal
          modalRef={this.deployScriptModalRef}
          projectSettings={projectSettings}
          projectManager={projectManager}
        />
        <ExpressionManagerModal
          modalRef={this.expressionManagerModal}
          currentTab={this.state.currentTab}
          isOpen={this.state.isExpressionManagerModalOpen}
          close={() => this.setState({ isExpressionManagerModalOpen: false })}
          managerType={this.state.expressionManagerType}
          projectManager={projectManager}
        />
      </>
    );
  }
}
