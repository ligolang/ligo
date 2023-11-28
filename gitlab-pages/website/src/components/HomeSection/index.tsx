import Title, { Subtitle } from "@site/src/components/Titles";
import clsx from "clsx";
import React, { forwardRef } from "react";
import styles from "./styles.module.scss";

type HomeSectionProps = {
  children: React.ReactNode;
  title: string;
  subtitle: string;
  id: string;
};

const HomeSection = forwardRef((props: HomeSectionProps, ref: React.RefObject<HTMLElement>) => {
  const { children, title, subtitle, id } = props;
  return (
    <section id={id} className={clsx(styles.home__section, "reveal")} ref={ref}>
      <Title level={2}>{title}</Title>
      <Subtitle>{subtitle}</Subtitle>
      {children}
    </section>
  );
});

export default HomeSection;
