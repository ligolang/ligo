import React from "react";

type ImageProps = React.ComponentPropsWithoutRef<"img">;

const Image = (props: ImageProps) => {
  return (
    <img {...props} loading="lazy" decoding="async" alt={props.alt || ""}>
      {props.children}
    </img>
  );
};

export default Image;
