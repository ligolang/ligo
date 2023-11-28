import React from "react";

type TitleProps = {
  level: 1 | 2 | 3 | 4 | 5 | 6;
  children: React.ReactNode;
  className?: string;
};
type SubtitleProps = Omit<TitleProps, "level">;

export const Subtitle = ({ children, className }: SubtitleProps) => (
  <p className={className}>{children}</p>
);

const Title = (props: TitleProps) => {
  switch (props.level) {
    case 1:
      return <h1 className={props.className}>{props.children}</h1>;
    case 2:
      return <h2 className={props.className}>{props.children}</h2>;
    case 3:
      return <h3 className={props.className}>{props.children}</h3>;
    case 4:
      return <h4 className={props.className}>{props.children}</h4>;
    case 5:
      return <h5 className={props.className}>{props.children}</h5>;
    case 6:
      return <h6 className={props.className}>{props.children}</h6>;
  }
};

export default Title;
