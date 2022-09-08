import React, { PureComponent } from "react";
import classnames from "classnames";

import { Input } from "reactstrap";
import ToolbarButton from "../buttons/ToolbarButton";
import { utils } from "~/ligo-components/eth-sdk";

export default class NavigationBar extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      value: utils.isValidAddressReturn(props.tab.value),
    };
    this.input = React.createRef();
  }

  componentDidUpdate(prevProps) {
    this.setState({ value: utils.isValidAddressReturn(this.state.value) });
    if (prevProps.tab.key === this.props.tab.key) {
      return;
    }
    prevProps.tab.selection = this.selection;

    const { value, temp } = this.props.tab;
    this.setState({ value: temp || value || "" });
    setTimeout(this.fixFocus, 10);
  }

  fixFocus = () => {
    const { value, selection } = this.props.tab;
    if (selection) {
      this.input.current.focus();
      this.selection = selection;
    } else if (!value) {
      this.input.current.focus();
      this.input.current.select();
    }
  };

  get selection() {
    const input = this.input.current;
    if (input === document.activeElement) {
      return {
        start: input.selectionStart,
        end: input.selectionEnd,
        direction: input.selectionDirection,
      };
    }
    return null;
  }

  set selection(selection) {
    const input = this.input.current;
    if (!selection) {
      input.blur();
      return;
    }
    input.focus();
    const { start, end, direction } = selection;
    input.selectionStart = start;
    input.selectionEnd = end;
    input.selectionDirection = direction;
  }

  onChange = (event) => {
    const value = utils.isValidAddressReturn(event.target.value);
    this.setState({ value });
    this.props.tab.temp = value.toLowerCase();
  };

  onKeyPress = (event) => {
    if (event.key === "Enter") {
      let { value } = event.target;
      if (value.trim() !== value) {
        value = value.trim();
        this.setState({ value });
      }
      this.props.updateTab({ value: value.toLowerCase(), temp: undefined });
      this.input.current.blur();
      this.forceUpdate();

      if (this.props.onEnter) {
        this.props.onEnter(value.toLowerCase());
      }
    }
  };

  noSelection = false;

  onMouseDown = () => {
    this.noSelection = !this.selection;
  };

  onMouseUp = (event) => {
    if (this.noSelection) {
      if (this.selection && this.selection.start === this.selection.end) {
        event.target.select();
      }
    }
  };

  selectionCache = null;

  onBlur = (event) => {
    const selection = {
      start: event.target.selectionStart,
      end: event.target.selectionEnd,
      direction: event.target.selectionDirection,
    };
    this.selectionCache = selection;
  };

  recoverSelectionCache = () => {
    this.selection = this.selectionCache;
  };

  renderStar = (starred) => {
    if (this.props.tab.temp || !this.state.value) {
      return null;
    }

    if (starred) {
      return (
        <div
          key="narbar-star"
          className="btn btn-sm text-warning hover-block"
          onClick={() => this.props.onToggleStar(this.state.value.toLowerCase(), false)}
        >
          <i className="fas fa-star hover-hide" />
          <i className="fas fa-star hover-show" />
        </div>
      );
    }
    return (
      <div
        key="narbar-star-o"
        className="btn btn-sm text-secondary hover-block"
        onClick={() => this.props.onToggleStar(this.state.value.toLowerCase(), true)}
      >
        <i className="fas fa-star hover-hide" />
        <i className="fas fa-star hover-show" />
      </div>
    );
  };

  render() {
    const { size = "md", maxLength, disabled, children } = this.props;

    return (
      <div className="d-flex align-items-center border-bottom-1 ">
        <ToolbarButton
          id="navbar-refresh"
          size={size}
          icon="fas fa-redo-alt"
          tooltip="Refresh"
          onClick={this.props.onRefresh}
        />
        <div
          className={classnames(
            "d-flex flex-1 align-items-center navbar-input-wrapper ml-1",
            children ? "mr-1" : "mr-2"
          )}
        >
          <div
            key={`address-icon-${this.state.value ? "" : "none"}`}
            className="btn btn-sm text-secondary pr-1"
          >
            <i className={this.state.value ? "fas fa-map-marker-alt" : "fas fa-map-marker"} />
          </div>
          <Input
            innerRef={this.input}
            bsSize={size}
            className="navbar-input btn-sm code"
            maxLength={maxLength}
            value={this.state.value}
            disabled={disabled}
            onChange={this.onChange}
            onKeyPress={this.onKeyPress}
            onMouseDown={this.onMouseDown}
            onMouseUp={this.onMouseUp}
            onBlur={this.onBlur}
          />
          {this.renderStar(this.props.starred)}
        </div>
        {children}
      </div>
    );
  }
}
