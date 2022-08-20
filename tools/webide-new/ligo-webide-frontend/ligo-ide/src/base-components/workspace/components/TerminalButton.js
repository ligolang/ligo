import React, { PureComponent } from "react";

import { ToolbarButton } from "~/base-components/ui-components";
import LocalProjectManager from "../ProjectManager/LocalProjectManager";

export default class TerminalButton extends PureComponent {
  state = {
    terminal: false,
  };

  componentDidMount() {
    LocalProjectManager.terminalButton = this;
  }

  render() {
    const { size = "sm" } = this.props;

    return (
      <ToolbarButton
        id="terminal"
        size={size}
        icon="fas fa-terminal"
        color={this.state.terminal ? "primary" : "default"}
        onClick={() => LocalProjectManager.instance.toggleTerminal(!this.state.terminal)}
      />
    );
  }
}
