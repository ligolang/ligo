import React, { PureComponent } from "react";

import { WorkspaceContext } from "~/base-components/workspace";
import { ToolbarButton, DropdownToolbarButton } from "~/base-components/ui-components";
import keypairManager from "~/base-components/keypair";
import DeployScriptModal from "./DeployScriptModal";
import DeployModal from "./DeployModal";
import CompileModal from "./CompileModal";
import ExpressionManagerModal from "./ExpressionManagerModal";
import { networkManager } from "~/ligo-components/eth-network";
import notification from "~/base-components/notification";

import DeployButton from "./DeployButton";
import SignRequestModal from "./SignRequestModal";
import fileOps from "~/base-components/file-ops";

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
      isPreDeploy: false,
    };
  }

  deployScriptModal = () => {
    this.deployScriptModalRef.current.openModal();
  };

  deployModal = async () => {
    if (!networkManager.sdk) {
      notification.error("Cannot Deploy", "No connected network.");
      return;
    }
    const deployPath = this.context.projectSettings?.get("deploy") || "";
    if (!(await fileOps.exists(this.context.projectManager.pathForProjectFile(deployPath)))) {
      this.setState({ isPreDeploy: true });
      this.compileModalOpen();
    } else {
      this.deployModalRef.current.openModal();
    }
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
    const { signer, noBuild, noDeploy, ExtraButtons = () => null, isExpanded } = this.props;
    const { projectSettings, projectManager } = this.context;
    const compilers = projectSettings?.get("compilers") || {};
    const readOnly = !projectManager.userOwnProject && projectManager.remote;

    return (
      <>
        <ToolbarButton
          id="compile"
          icon="fas fa-play"
          tooltip="Compile"
          readOnly={readOnly}
          onClick={() => this.compileModalOpen()}
          isExpanded={isExpanded}
        />
        <ToolbarButton
          id="deploy"
          icon="fas fa-plane-departure"
          tooltip="Deploy"
          readOnly={readOnly}
          onClick={() => this.deployModal()}
          isExpanded={isExpanded}
        />
        <ToolbarButton
          id="deploy-script"
          icon="fas fa-file-export"
          tooltip="Deploy Script"
          readOnly={readOnly}
          onClick={() => this.deployScriptModal()}
          isExpanded={isExpanded}
        />
        <ToolbarButton
          id="dry-run"
          icon="fas fa-sun"
          tooltip="Dry Run"
          readOnly={readOnly}
          onClick={() => this.expressionExecutionModal("dryRun")}
          isExpanded={isExpanded}
        />
        <ToolbarButton
          id="compile-expr"
          icon="fas fa-wrench"
          tooltip="Compile Expression"
          readOnly={readOnly}
          onClick={() => this.expressionExecutionModal("compile")}
          isExpanded={isExpanded}
        />
        <ExtraButtons projectManager={projectManager} signer={signer} />
        <div className="flex-1" />
        <ToolbarButton
          id="settings"
          icon="fas fa-cog"
          tooltip="Project Settings"
          onClick={() => projectManager.openProjectSettings()}
          isExpanded={isExpanded}
        />
        <SignRequestModal ref={keypairManager.signReqModal} />
        <CompileModal
          modalRef={this.compileModalRef}
          tzFilePath={this.state.tzFilePath}
          onCompile={() => {
            projectManager.compile(null, this.props.finalCall);
            this.setState({ isPreDeploy: false });
          }}
          isPreDeploy={this.state.isPreDeploy}
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
        <DeployModal
          modalRef={this.deployModalRef}
          projectSettings={projectSettings}
          projectManager={projectManager}
          signer={signer}
        />
      </>
    );
  }
}
