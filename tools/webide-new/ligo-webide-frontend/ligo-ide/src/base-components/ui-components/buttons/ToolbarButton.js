import React from "react";
import classnames from "classnames";

import { Button, UncontrolledTooltip } from "reactstrap";

export default function ToolbarButton({
  id,
  color = "default",
  size = "sm",
  rounded = undefined,
  onClick,
  icon,
  loading = undefined,
  tooltip = null,
  tooltipPlacement = "bottom",
  className = undefined,
  children = undefined,
  readOnly = undefined,
  isExpanded = false,
}) {
  const childrenComponent = loading ? (
    <span key="loading">
      <i className="fas fa-spin fa-spinner" />
    </span>
  ) : (
    children || (
      <>
        <span className={isExpanded ? "ml-2 mr-2" : ""} key="icon">
          <i className={icon} />
        </span>
        {isExpanded ? tooltip : null}
      </>
    )
  );
  const tooltipComponent = tooltip && (
    <UncontrolledTooltip
      trigger="hover"
      delay={0}
      placement={tooltipPlacement}
      target={`toolbar-btn-${id}`}
    >
      {tooltip}
    </UncontrolledTooltip>
  );

  return (
    <>
      <Button
        size={size}
        color={color}
        id={`toolbar-btn-${id}`}
        key={`toolbar-btn-${id}`}
        className={classnames(
          isExpanded
            ? "d-flex align-items-start"
            : "flex-none px-2 w-5 flex-column align-items-center",
          !rounded && "rounded-0 border-0",
          className
        )}
        onClick={onClick}
        disabled={readOnly}
      >
        {childrenComponent}
      </Button>
      {!isExpanded && tooltipComponent}
    </>
  );
}
