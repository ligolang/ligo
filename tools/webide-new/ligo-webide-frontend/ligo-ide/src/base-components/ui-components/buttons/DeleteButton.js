import React, { PureComponent } from "react";
import PropTypes from "prop-types";
import classnames from "classnames";

import { Tooltip } from "reactstrap";

import IconButton from "./IconButton";

const deleteButtons = {};

export default class DeleteButton extends PureComponent {
  static propTypes = {
    size: PropTypes.string,
    textConfirm: PropTypes.string,
    onConfirm: PropTypes.func,
    className: PropTypes.string,
  };

  state = {
    confirming: false,
  };

  constructor(props) {
    super(props);
    this.id = Math.floor(Math.random() * 10000);
    deleteButtons[this.id] = this;
  }

  componentWillUnmount() {
    delete deleteButtons[this.id];
    this.setState({ confirming: false });
    clearTimeout(this.timeout);
  }

  showConfirm = () => {
    Object.keys(deleteButtons).forEach(id => {
      deleteButtons[id] && deleteButtons[id].setState({ confirming: false });
    });
    this.setState({ confirming: true });
    this.timeout = setTimeout(() => {
      this.setState({ confirming: false });
    }, 2000);
  };

  onConfirm = () => {
    this.setState({ confirming: false });
    this.props.onConfirm && this.props.onConfirm();
  };

  render() {
    const {
      color = "danger",
      textConfirm = "Click again to delete",
      icon = "fas fa-trash-alt",
      className = "",
    } = this.props;

    if (this.state.confirming) {
      return (
        <IconButton
          color={color}
          id={`confirm-delete-${this.id}`}
          className={className}
          icon={icon}
          onClick={this.onConfirm}
        >
          <Tooltip placement="top" isOpen target={`confirm-delete-${this.id}`}>
            <i className="fas fa-exclamation-circle" /> {textConfirm}
          </Tooltip>
        </IconButton>
      );
    }
    return (
      <IconButton
        color="transparent"
        id={`confirm-delete-${this.id}`}
        className={classnames("text-muted", className)}
        icon={icon}
        onClick={this.showConfirm}
      />
    );
  }
}
