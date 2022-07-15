import React, { forwardRef, useMemo } from "react";
import PropTypes from "prop-types";
import classnames from "classnames";

import { Label, FormGroup, UncontrolledTooltip, Button } from "reactstrap";

import DebouncedInput from "./DebouncedInput";

export default forwardRef(DebouncedFormGroup);

function LabelTooltip(props) {
  if (!props.tooltip) {
    return null;
  }

  const id = useMemo(() => `tooltip-input-${Math.floor(Math.random() * 10000)}`, []);
  return (
    <>
      <span
        id={id}
        key={id}
        className={classnames(props.size === "sm" ? "small ml-1" : "ml-2", "text-muted")}
      >
        <i className="fas fa-info-circle" />
      </span>
      <UncontrolledTooltip target={id}>{props.tooltip}</UncontrolledTooltip>
    </>
  );
}

function execValidator(value, validator) {
  if (!value || !validator) {
    return [undefined, ""];
  }
  const feedback = validator(value) || "";
  return [!!feedback, feedback];
}

function DebouncedFormGroup(props, ref) {
  const {
    size,
    label,
    disabled,
    placeholder,
    inputType = "input",
    onTextClick,
    formGroupClassName,
    validator,
    importFromFile,
    ...otherProps
  } = props;

  const [invalid, feedback] = execValidator(otherProps.value, validator);

  const onChange = value => {
    const [invalid] = execValidator(value, validator);
    otherProps.onChange(value, invalid);
  };

  const onChooseFile = () => {
    const input = document.createElement("input");
    input.type = "file";
    if (typeof importFromFile === "string") input.accept = importFromFile;
    input.onchange = event => {
      const file = input.files[0];
      const fr = new FileReader();
      fr.onload = event => {
        onChange(event.target.result);
      };
      fr.readAsText(file);
    };
    input.click();
  };

  return (
    <FormGroup className={classnames(size === "sm" && "mb-2", formGroupClassName)}>
      <Label className={classnames(size === "sm" && "mb-1 small")}>
        {label}
        {importFromFile && (
          <Button color="secondary" size="sm" className="ml-2" onClick={onChooseFile}>
            Import from file {typeof importFromFile === "string" && `(${importFromFile})`}
          </Button>
        )}
      </Label>
      <LabelTooltip tooltip={props.tooltip} size={size} />
      {inputType === "input" && (
        <DebouncedInput
          ref={ref}
          size={size}
          disabled={disabled}
          placeholder={placeholder}
          invalid={invalid}
          feedback={feedback}
          {...otherProps}
          onChange={onChange}
        />
      )}
      {inputType === "text" && (
        <div className={onTextClick ? "cursor-pointer" : ""}>
          <pre onClick={onTextClick} className="break-word">
            {placeholder}
          </pre>
        </div>
      )}
    </FormGroup>
  );
}

DebouncedFormGroup.propTypes = {
  label: PropTypes.node,
};
