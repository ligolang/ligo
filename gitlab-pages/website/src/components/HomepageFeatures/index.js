import React from 'react';
import useBaseUrl from '@docusaurus/useBaseUrl';

const FEATURES = [
  {
    image: 'img/ligo_features/strong-type-system.svg',
    title: 'Strong, Static Type System',
    content: 'Write types, then code. Benefit from the safety of type systems.'
  },
  {
    image: 'img/ligo_features/syntax-agnostic.svg',
    title: 'Polyglot',
    content:
      'Code in your language. Write PascaLIGO, CameLIGO, JsLIGO or add your own syntax.'
  },
  {
    image: 'img/ligo_features/easy-integration.svg',
    title: 'Easy Integration',
    content: 'You can use LIGO as a Node.js library with Truffle.'
  }
];

const Feature = (props) => (
  <div className="feature" key={props.title}>
    <img src={useBaseUrl(props.image)} />
    <h1>{props.title}</h1>
    <p>{props.content}</p>
  </div>
);


export default function HomepageFeatures() {
  return (
    <div id="features" className="centered">
      {FEATURES.map(entry =>
        <Feature key={entry.title} title={entry.title} content={entry.content} image={entry.image} />
      )}
    </div>
  );
}
