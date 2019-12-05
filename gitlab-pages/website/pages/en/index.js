const React = require('react');
const MarkdownBlock = require('../../core/CompLibrary').MarkdownBlock;
const CodeExamples = require(`${process.cwd()}/core/CodeExamples`);
const docUrl = require(`${process.cwd()}/core/UrlUtils`).docUrl;

const FEATURES = [
  {
    image: 'img/strong-type-system.svg',
    title: 'Strong Type System',
    content: 'Write types, then code. Benefit from the safety of type systems.'
  },
  {
    image: 'img/syntax-agnostic.svg',
    title: 'Syntax Agnostic',
    content:
      'Code in your language. Write PascaLIGO, CameLIGO, or add your own syntax.'
  },
  {
    image: 'img/easy-integration.svg',
    title: 'Easy Integration',
    content: 'You can use LIGO as a NodeJS library with Granary'
  }
];

const PARTNERS = [
  {
    name: 'Nomadic Labs',
    image: 'img/nomadic-logo.png',
    link: 'https://www.nomadic-labs.com/',
    pinned: true
  },
  {
    name: 'Tocqueville Group',
    image: 'img/tq-logo.svg',
    link: 'https://tqgroup.io/',
    pinned: true
  },
  {
    name: 'Stove Labs',
    image: 'img/stove-logo.png',
    link: 'https://stove-labs.com',
    pinned: true
  }
];

const Feature = props => (
  <div className="feature" key={props.title}>
    <img src={props.image} />
    <h1>{props.title}</h1>
    <p>{props.content}</p>
  </div>
);

const Partner = props => (
  <a
    href={props.link}
    title={props.name}
    target="_blank"
    rel="noopener noreferrer"
  >
    <img src={props.image} />
  </a>
);

module.exports = props => {
  return (
    <div id="homePage">
      <div id="intro" className="centered">
        <div id="callToAction">
          <ul>
            <li className="primary">
              <a href="https://ide.ligolang.org">Try Online</a>
            </li>
            <li className="secondary">
              <a href={docUrl(props.config, 'intro/installation')}>Install</a>
            </li>
          </ul>
        </div>
        <div id="preview">
          <h1>A friendly smart-contract language for Tezos</h1>
          <p>Michelson was never so easy</p>
          <CodeExamples MarkdownBlock={MarkdownBlock}></CodeExamples>
        </div>
      </div>
      <div id="features" className="centered">
        {FEATURES.map(Feature)}
      </div>
      <div id="partners">
        <div className="centered wrapper">
          <span id="heading">Our Partners</span>
          <div id="list">
            {PARTNERS.filter(entry => entry.pinned).map(Partner)}
          </div>
        </div>
      </div>
    </div>
  );
};
