import React from "react";
import { Label } from "reactstrap";

export default function FormSection(props) {
  const { title, right, children } = props;

  return (
    <>
      <Label className="d-flex align-items-center justify-content-between">
        <div>{title}</div>
        <div className="d-flex">{right}</div>
      </Label>
      <div className="mb-3">{children}</div>
    </>
  );
}
