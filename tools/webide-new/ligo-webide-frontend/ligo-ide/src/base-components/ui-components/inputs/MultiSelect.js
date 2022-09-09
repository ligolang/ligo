import React, { PureComponent } from "react";
import classnames from "classnames";

import { InputGroupAddon, Button } from "reactstrap";

import Select, { components } from "react-select";

import "./multiselect.scss";

export default class MultiSelect extends PureComponent {
  handleChange = (selectedOption) => {
    this.setState({ selectedOption }, () => {
      // console.log(`Option selected:`, this.state.selectedOption)
    });
  };

  onChange = async (items, change) => {
    switch (change.action) {
      case "select-option":
        try {
          const { option } = change;
          const value = option.getValue ? await option.getValue() : option;
          this.props.onChange([...this.props.value, value]);
        } catch (e) {
          console.warn(e);
        }
        return;
      case "remove-value":
      case "pop-value":
      case "clear":
        this.props.onChange(items || []);
        break;
    }
  };

  onClickLabel = async (data) => {
    const newLabel = await this.props.onClickLabel(data);
    if (newLabel) {
      const index = this.props.value.indexOf(data);
      if (index > -1) {
        this.props.value[index] = newLabel;
        this.props.onChange([...this.props.value]);
      }
    }
  };

  render() {
    const { size, prepend, append, onClick, value, options, placeholder } = this.props;

    return (
      <Select
        isMulti
        isSearchable={false}
        isClearable={false}
        className="react-select-container flex-1"
        classNamePrefix="react-select"
        components={{
          Control: (props) => {
            const onMouseDown = onClick
              ? async () => props.selectOption(await onClick())
              : props.innerProps.onMouseDown;
            return (
              <div onMouseDown={onMouseDown} className="input-group input-group-sm">
                {prepend ? (
                  <InputGroupAddon addonType="prepend">
                    <Button
                      color="secondary"
                      className={classnames(size === "sm" ? "px-0" : "px-1")}
                    >
                      <div className="w-5">{prepend}</div>
                    </Button>
                  </InputGroupAddon>
                ) : null}
                {props.children}
                {append ? (
                  <InputGroupAddon addonType="append">
                    <Button color="secondary">{append}</Button>
                  </InputGroupAddon>
                ) : null}
              </div>
            );
          },
          ValueContainer: (props) => (
            <div className="form-control h-auto">
              <components.ValueContainer {...props} className="p-0" />
            </div>
          ),
          MultiValueLabel: (props) => (
            <div
              {...props.innerProps}
              onClick={() => this.onClickLabel(props.data)}
              onMouseDown={(event) => event.stopPropagation()}
            >
              {props.children}
            </div>
          ),
          IndicatorsContainer: (props) =>
            options ? <div className="input-group-append">{props.children}</div> : null,
          DropdownIndicator: () => <button className="dropdown-toggle btn btn-secondary" />,
          IndicatorSeparator: () => null,
          Menu: (props) => (
            <div
              {...props.innerProps}
              ref={props.ref}
              className="dropdown-menu dropdown-menu-right show"
              style={{ boxShadow: "none", width: "auto" }}
            >
              {props.children}
            </div>
          ),
          MenuList: (props) => <components.MenuList {...props} className="p-0" />,
          Group: (props) => <components.Group {...props} className="p-0" />,
          GroupHeading: (props) => <h6 className="dropdown-header">{props.children}</h6>,
          Option: (props) =>
            props.data.type === "divider" ? (
              <div className="dropdown-divider" />
            ) : (
              <div className="dropdown-item" {...props.innerProps}>
                {props.children}
              </div>
            ),
        }}
        value={value}
        options={options}
        onChange={this.onChange}
        placeholder={placeholder}
      />
    );
  }
}
