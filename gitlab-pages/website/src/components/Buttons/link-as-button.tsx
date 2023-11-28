import React from "react";
import styles from "./link-as-button.module.css";

type Variant = "primary" | "ghost";

type LinkAsButtonProps = React.ComponentPropsWithoutRef<"a"> & {
  variant?: Variant;
};

const getClassNameUsingVariant = (variant: string) => {
  return `btn_variant--${variant}`;
};

const LinkAsButton = ({ variant = "primary", ...props }: LinkAsButtonProps) => {
  return (
    <a
      {...props}
      type="button"
      className={`${styles.btn} ${styles[getClassNameUsingVariant(variant)]}`}
    >
      {props.children}
    </a>
  );
};

export default LinkAsButton;
