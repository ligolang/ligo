import React, { PureComponent } from "react";

import { ToolbarButton } from "~/base-components/ui-components";
import ProjectManager from "../ProjectManager/ProjectManager";

export default class TerminalButton extends PureComponent {
  state = {
    terminal: false,
  };

  componentDidMount() {
    ProjectManager.terminalButton = this;
  }

  render() {
    const { size = "sm" } = this.props;

    return (
      <ToolbarButton
        id="terminal"
        size={size}
        icon="fas fa-terminal"
        color={this.state.terminal ? "primary" : "default"}
        onClick={() => ProjectManager.instance.toggleTerminal(!this.state.terminal)}
      />
    );
  }
}
