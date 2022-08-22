import React, { PureComponent } from "react";

import { WorkspaceContext } from "~/base-components/workspace";
import { ToolbarButton, DropdownToolbarButton } from "~/base-components/ui-components";
import { CompilerButton } from "~/ligo-components/eth-compiler";
import keypairManager from "~/base-components/keypair";
import GistUploadModals from "~/base-components/workspace/components/GistUploadModals";
import DeployScriptModal from "./DeployScriptModal";
import ExpressionManagerModal from "./ExpressionManagerModal";

// import DeployButton from './DeployButton'
import SignRequestModal from "./SignRequestModal";

export default class ProjectToolbar extends PureComponent {
  // eslint-disable-next-line react/static-property-placement
  static contextType = WorkspaceContext;

  constructor(props) {
    super(props);
    this.deployScriptModal = React.createRef();
    this.expressionManagerModal = React.createRef();
    this.state = {
      isExpressionManagerModalOpen: false,
      currentTab: "",
      expressionManagerType: "",
    };
  }

  gistUploadFileModal = () => {
    this.deployScriptModal.current.openModal();
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
        {!noBuild && (
          <CompilerButton
            className="rounded-0 border-0 flex-none w-5"
            truffle={compilers[process.env.COMPILER_VERSION_KEY]}
            solc={compilers.solc}
            onClick={() => projectManager.compile(null, this.props.finalCall)}
            readOnly={readOnly}
          />
        )}
        <ToolbarButton
          id="deploy-script"
          icon="fas fa-file-export"
          tooltip="Deploy Script"
          readOnly={readOnly}
          onClick={() => this.gistUploadFileModal()}
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
        <DeployScriptModal
          modalRef={this.deployScriptModal}
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
