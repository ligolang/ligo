import React from "react";

import { connect } from "react-redux";
import { DebouncedFormGroup, FormGroup, Label, Input } from "~/base-components/ui-components";

import {
  WorkspaceContext,
  ProjectManager,
  AbstractProjectSettingsTab,
  ProjectPath,
} from "~/base-components/workspace";

class ProjectSettingsTab extends AbstractProjectSettingsTab {
  static contextType = WorkspaceContext;

  componentDidMount() {
    ProjectManager.channel.on("settings", this.debouncedUpdate);
  }

  componentWillUnmount() {
    ProjectManager.channel.off("settings", this.debouncedUpdate);
  }

  render() {
    const { projectRoot, projectManager, projectSettings } = this.context;
    const readOnly = !projectManager.userOwnProject && projectManager.remote;

    return (
      <div className="custom-tab bg2">
        <div className="jumbotron bg-transparent text-body">
          <div className="container">
            <h1>Project Settings</h1>
            <form disabled>
              <ProjectPath projectRoot={projectRoot} remote={projectManager.remote} />

              <h4 className="mt-4">General</h4>
              <DebouncedFormGroup
                label="Main file"
                className="bg-black"
                value={projectSettings?.get("main")}
                onChange={this.onChange("main")}
                placeholder="Required"
                readOnly={readOnly}
              />
              <DebouncedFormGroup
                label="Smart contract to deploy"
                className="bg-black"
                value={projectSettings?.get("deploy")}
                onChange={this.onChange("deploy")}
                placeholder="Path to the built contract to deploy"
                readOnly={readOnly}
              />
              <DebouncedFormGroup
                label="Contract storage (optional)"
                className="bg-black"
                value={projectSettings?.get("storage")}
                onChange={this.onChange("storage")}
                placeholder="Path to the contract storage"
                readOnly={readOnly}
              />
              <DebouncedFormGroup
                label="Module name (optional)"
                className="bg-black"
                value={projectSettings?.get("module")}
                onChange={this.onChange("module")}
                placeholder="Name of module contract"
                readOnly={readOnly}
              />
              <h4 className="mt-4">Gist</h4>
              <DebouncedFormGroup
                label="Gist token (not stored in config)"
                className="bg-black"
                value={this.props.gistToken}
                onChange={(t) => this.props.dispatch({ type: "SET_GIST_TOKEN", payload: t })}
                placeholder="Token"
                readOnly={readOnly}
              />
              <DebouncedFormGroup
                label="Gist ID"
                className="bg-black"
                value={projectSettings?.get("gistId")}
                onChange={this.onChange("gistId")}
                placeholder="Gist id"
                readOnly={readOnly}
              />
              <h4 className="mt-4">Project workflow</h4>
              <FormGroup className="actionConfirm__checkbox">
                <div className="ml-4">
                  <Input
                    type="checkbox"
                    id="compilation-info-check-box"
                    disabled={readOnly}
                    onChange={() =>
                      this.onChange("doNotShowCompilationMessage")(
                        !projectSettings?.get("doNotShowCompilationMessage")
                      )
                    }
                    checked={projectSettings?.get("doNotShowCompilationMessage")}
                  />
                  <Label check htmlFor="compilation-info-check-box">
                    Do not show compilation warning message
                  </Label>
                </div>
              </FormGroup>
            </form>
          </div>
        </div>
      </div>
    );
  }
}

const mapStateToProps = function (state) {
  return {
    gistToken: state.gistToken,
  };
};

export default connect(mapStateToProps)(ProjectSettingsTab);
