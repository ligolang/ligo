import React from "react";
import classnames from "classnames";

import { ButtonDropdown, DropdownToggle, DropdownMenu, DropdownItem } from "reactstrap";

import ToolbarButton from "./ToolbarButton";

export default function DropdownToolbarButton({
  id,
  color = "default",
  size = "sm",
  rounded,
  icon,
  options = [],
  className,
}) {
  const [isOpen, setOpen] = React.useState(false);
  const toggle = () => setOpen(!isOpen);

  let dropdownItems;
  if (options.length) {
    dropdownItems = options.map((opt) => (
      <DropdownItem key={opt.key} onClick={opt.onClick}>
        {opt.key}
      </DropdownItem>
    ));
  } else {
    dropdownItems = <DropdownItem disabled>(None)</DropdownItem>;
  }

  return (
    <ButtonDropdown isOpen={isOpen} toggle={toggle}>
      <DropdownToggle tag="div">
        <ToolbarButton
          id={id}
          color={color}
          size={size}
          rounded={rounded}
          onClick={toggle}
          icon={icon}
          className={className}
        />
      </DropdownToggle>
      <DropdownMenu className="dropdown-menu-sm">
        <DropdownItem header>scripts</DropdownItem>
        {dropdownItems}
      </DropdownMenu>
    </ButtonDropdown>
  );
}
