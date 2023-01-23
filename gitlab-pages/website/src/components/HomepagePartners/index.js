import React from 'react';
import useBaseUrl from '@docusaurus/useBaseUrl';


const PARTNERS = [
  {
    name: 'Serokell',
    image: 'img/partners/serokell-dark-logo.svg',
    link: 'https://serokell.io/',
    pinned: true
  },{
    name: 'Nomadic Labs',
    image: 'img/partners/nomadic-logo.png',
    link: 'https://www.nomadic-labs.com/',
    pinned: true
  },
  {
    name: 'MacStadium developer logo',
    image: 'img/partners/MacStadium-developerlogo.png',
    link: 'https://www.macstadium.com/',
    pinned: true
  },
  {
    name: 'TQ Tezos',
    image: 'img/partners/tq-logo.svg',
    link: 'https://tqtezos.com/',
    pinned: true
  },
  {
    name: 'Stove Labs',
    image: 'img/partners/stove-logo.png',
    link: 'https://stove-labs.com',
    pinned: true
  }
];

const Partner = (props) => (
  <a
    href={props.link}
    title={props.name}
    target="_blank"
    rel="noopener noreferrer"
  >
    <img src={useBaseUrl(props.image)} />
  </a>
);

export default function HomepagePartners() {
  return (
    <div id="partners">
      <div className="centered wrapper">
        <span id="heading">Our Partners</span>
        <div id="list">
          {PARTNERS.filter(entry => entry.pinned).map(entry =>
            <Partner key={entry.name} name={entry.name} image={entry.image} link={entry.link} />
          )}
        </div>
      </div>
    </div>
  );
}

