import MacStadium from "@site/src/components/Icons/MacStadium";
import NomadicLabs from "@site/src/components/Icons/NomadicLabs";
import React from "react";
import Serokell from "../Icons/Serokell";
import styles from "./styles.module.css";

const PARTNERS = [
  {
    name: "Serokell",
    image: <Serokell />,
    link: "https://serokell.io/",
    pinned: true,
  },
  {
    name: "Nomadic Labs",
    image: <NomadicLabs />,
    link: "https://www.nomadic-labs.com/",
    pinned: true,
  },
  {
    name: "MacStadium developer logo",
    image: <MacStadium />,
    link: "https://www.macstadium.com/",
    pinned: true,
  },
];

const Partner = (props) => (
  <a
    href={props.link}
    className={styles.partner}
    title={props.name}
    target="_blank"
    rel="noopener noreferrer"
  >
    {props.image}
  </a>
);

export default function HomepagePartners() {
  return (
    <div id="partners" className={styles.partners}>
      {PARTNERS.filter((entry) => entry.pinned).map((entry) => (
        <Partner key={entry.name} name={entry.name} image={entry.image} link={entry.link} />
      ))}
    </div>
  );
}
