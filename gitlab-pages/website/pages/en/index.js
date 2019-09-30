/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require("react");

const CompLibrary = require("../../core/CompLibrary.js");

const hljs = require("highlight.js");

const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

function pascaligo(hljs) {
  return {
    // case_insensitive: true,
    beginKeywords: '',
    keywords: {
      keyword: 'and begin block case const contains down else end fail for ' +
        'from function if in is list map mod nil not of or patch ' +
        'procedure record remove set skip step then to type var while with',
      literal: 'true false unit int string some none bool nat list'
    },
    lexemes: '[a-zA-Z][a-zA-Z0-9_]*',
    contains: [
      hljs.C_LINE_COMMENT_MODE,

      {
        className: 'type',
        begin: /[A-Z][a-z]+/
      },
      {
        begin: /[*+-:;\(\)\{\}|\>\<]/,
        // className: 'ignore'
      }
    ]
  }
}

hljs.registerLanguage('pascaligo', pascaligo);


const pre = "```";

const pascaligoExampleSmall = `${pre}pascaligo
// variant defining pseudo multi-entrypoint 
// actions
type action is
| Increment of int
| Decrement of int

function add 
  (const a: int; const b: int): int is 
  block { skip } with a + b

function subtract 
  (const a: int; const b: int): int 
  is block { skip } with a - b

// real entrypoint that re-routes the flow 
// based on the action provided
function main 
  (const p: action; const s: int): 
  (list(operation) * int) is
 block { skip } 
  with ((nil : list(operation)),
    case p of
    | Increment(n) -> add(s, n)
    | Decrement(n) -> subtract(s, n)
  end)
${pre}`;

const pascaligoExample = `${pre}pascaligo
// variant defining pseudo multi-entrypoint actions
type action is
| Increment of int
| Decrement of int

function add (const a : int ; const b : int) : int is
 block { skip } with a + b

function subtract (const a : int ; const b : int) : int is
 block { skip } with a - b

// real entrypoint that re-routes the flow based 
// on the action provided
function main (const p : action ; const s : int) : 
  (list(operation) * int) is
 block { skip } with ((nil : list(operation)),
  case p of
  | Increment(n) -> add(s, n)
  | Decrement(n) -> subtract(s, n)
 end)
${pre}`;
const cameligoExampleSmall = `${pre}ocaml
type storage = int

(* variant defining pseudo multi-entrypoint 
  actions *)
type action =
  | Increment of int
  | Decrement of int

let add (a: int) (b: int): int = a + b

let subtract (a: int) (b: int): int = a - b

(* real entrypoint that re-routes the flow 
   based on the action provided *)
let%entry main(p : action) storage =
  let storage =
    match p with
    | Increment n -> add storage n
    | Decrement n -> subtract storage n
  in (([] : operation list), storage)
${pre}`;

const cameligoExample = `${pre}ocaml
type storage = int

(* variant defining pseudo multi-entrypoint actions *)
type action =
  | Increment of int
  | Decrement of int

let add (a: int) (b: int): int = a + b

let subtract (a: int) (b: int): int = a - b

(* real entrypoint that re-routes the flow based on 
   the action provided *)
let%entry main(p : action) storage =
  let storage =
    match p with
    | Increment n -> add storage n
    | Decrement n -> subtract storage n
  in (([] : operation list), storage)
${pre}`;

const PascalLIGOTab = () => (
  <div
    id="tab-group-3-content-4"
    className="tab-pane active code-snippet"
    data-group="group_3"
    tabIndex="-1"
  >
    <MarkdownBlock>{pascaligoExampleSmall}</MarkdownBlock>
    <MarkdownBlock>{pascaligoExample}</MarkdownBlock>
  </div>
);

const CamelLIGOTab = () => (
  <div
    id="tab-group-3-content-5"
    className="tab-pane code-snippet"
    data-group="group_3"
    tabIndex="-1"
  >
    <MarkdownBlock>{cameligoExampleSmall}</MarkdownBlock>
    <MarkdownBlock>{cameligoExample}</MarkdownBlock>
  </div>
);

const LinkButton = props => (
  <a href={props.href} target={props.target}>
    <button className={props.className}>{props.children}</button>
  </a>
);

class HomeSplash extends React.Component {
  render() {
    const { siteConfig, language = "" } = this.props;
    const { baseUrl, docsUrl } = siteConfig;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ""}`;
    const langPart = `${language ? `${language}/` : ""}`;
    const docUrl = doc => `${baseUrl}${docsPart}${langPart}${doc}`;

    const SampleCode = props => (
      <div className="sample-code-container">
        <div className="sample-code">
          <div className="tabs">
            <div className="nav-tabs">
              <div
                id="tab-group-3-tab-4"
                className="nav-link active"
                data-group="group_3"
                data-tab="tab-group-3-content-4"
              >
                PascaLIGO
              </div>
              <div
                className="nav-link"
                data-group="group_3"
                data-tab="tab-group-3-content-5"
              >
                CameLIGO
              </div>
              <div className="disabled">ReasonLIGO (coming soon) </div>
            </div>
            <div className="tab-content">
              {PascalLIGOTab()}
              {CamelLIGOTab()}
            </div>
          </div>
        </div>
      </div >
    );

    return (
      <div className="home-container">
        <div className="home-text">
          <div className="projectTitle">
            <img alt={siteConfig.title} src={`${siteConfig.baseUrl}img/logo.svg`} />
          </div>
          <h4 className="tagline-text">{siteConfig.tagline}</h4>
          <p className="body">{siteConfig.taglineSub}</p>
          <LinkButton
            href={docUrl("setup/installation.html")}
            className="large-primary-button"
          >
            Get Started
          </LinkButton>
        </div>
        <SampleCode />
      </div>
    );
  }
}

class Index extends React.Component {
  render() {
    const { config: siteConfig, language = "" } = this.props;
    const { baseUrl } = siteConfig;

    const Block = props => (
      <Container
        padding={["bottom", "top"]}
        id={props.id}
        background={props.background}
      >
        <GridBlock
          align="center"
          contents={props.children}
          layout={props.layout}
        />
      </Container>
    );

    const FeatureCallout = () => (
      <div
        className="productShowcaseSection paddingBottom"
        style={{ textAlign: "center" }}
      >
        <h2>Feature Callout</h2>
        <MarkdownBlock>These are features of this project</MarkdownBlock>
      </div>
    );

    const TryOut = () => (
      <Block id="try">
        {[
          {
            content:
              "To make your landing page more attractive, use illustrations! Check out " +
              "[**unDraw**](https://undraw.co/) which provides you with customizable illustrations which are free to use. " +
              "The illustrations you see on this page are from unDraw.",
            image: `${baseUrl}img/undraw_code_review.svg`,
            imageAlign: "left",
            title: "Wonderful SVG Illustrations"
          }
        ]}
      </Block>
    );

    const Description = () => (
      <Block background="dark">
        {[
          {
            content:
              "This is another description of how this project is useful",
            image: `${baseUrl}img/undraw_note_list.svg`,
            imageAlign: "right",
            title: "Description"
          }
        ]}
      </Block>
    );

    const LearnHow = () => (
      <Block background="light">
        {[
          {
            content:
              "Each new Docusaurus project has **randomly-generated** theme colors.",
            image: `${baseUrl}img/undraw_youtube_tutorial.svg`,
            imageAlign: "right",
            title: "Randomly Generated Theme Colors"
          }
        ]}
      </Block>
    );

    const FeatureCard = props => (
      <div className="card" key={props.title}>
        <img src={props.image} />
        <div className="card-text">
          <h4>{props.title}</h4>
          <p className="body">{props.content}</p>
        </div>
      </div>
    );

    const Features = () => (
      <div className="features">
        <h2>Features</h2>

        <div className="flex-inline-container">
          {[
            {
              content:
                "Write types, then code, and benefit from the safety coming from type systems.",
              image: `${baseUrl}img/strong-type-system.svg`,
              title: "Strong Type System"
            },
            {
              content:
                "Write in PascaLIGO (pascal-like syntax) or CameLIGO (caml-like syntax). If you know OCaml, you can also add your own syntax.",
              image: `${baseUrl}img/syntax-agnostic.svg`,
              title: "Syntax Agnostic"
            },

            {
              content: "With Granary, you can use LIGO as a lib from NodeJS.",
              image: `${baseUrl}img/easy-integration.svg`,
              title: "Easy Integration"
            }
          ].map(FeatureCard)}
        </div>
      </div>
    );

    const Roadmap = () => (
      <div className="roadmap">
        <Block background="light">
          {[
            {
              content:
                "<h4>June 2019</h4>" +
                "<em><ul>" +
                "<li>First public release</li>" +
                "<li>PascaLIGO and CameLIGO syntaxes</li>" +
                "<li>Docs and Tutorials</li>" +
                "<li>Integration testing in ReasonML/JS with Granary</li>" +
                "</ul></em>" +
                "<h4>July 2019</h4>" +
                "<em><ul>" +
                "<li>Try LIGO online editor</li>" +
                "<li>Unit testing toolkit</li>" +
                "<li>ReasonLIGO syntax support</li>" +
                "<li>Repository with best practices & patterns for LIGO</li>" +
                "</ul></em>" +
                "<h4>August 2019</h4>" +
                "<em>" +
                "Long term plans will be announced soon" +
                "</em>",
              image: ``,
              imageAlign: "right",
              title: "Roadmap"
            }
          ]}
        </Block>
      </div>
    );

    const Partners = () => {
      if ((siteConfig.partners || []).length === 0) {
        return null;
      }

      const PartnerShowcase = siteConfig.partners
        .filter(user => user.pinned)
        .map(user => (
          <a className="partner-link" href={user.infoLink} key={user.infoLink}>
            <img src={user.image} alt={user.caption} title={user.caption} />
          </a>
        ));

      return (
        <div className="partners-container hide-small">
          {PartnerShowcase}
          <div className="partners-text">
            <h3>Our Partners</h3>
            <p className="body">
              We are not alone in this world -- here're some people who support us
            </p>
          </div>
        </div>
      );
    };

    const Team = () => {
      if ((siteConfig.team || []).length === 0) {
        return null;
      }

      const showcase = siteConfig.team
        .filter(user => user.pinned)
        .map(user => (
          <a
            className="profileContainer"
            href={user.infoLink}
            key={user.infoLink}
          >
            <img className="profileImage" src={user.image} alt={user.caption} />
            <p className="headline">{user.caption}</p>
          </a>
        ));

      return (
        <div className="team">
          <h2>Team</h2>
          <div className="flex-inline-container">{showcase}</div>
        </div>
      );
    };

    return (
      <div className="landing">
        <HomeSplash siteConfig={siteConfig} language={language} />
        <div className="mainContainer">
          <Features />
          {/* <Roadmap /> */}
          {/* <FeatureCallout /> */}
          {/* {/* <LearnHow /> */}
          {/* <TryOut /> */}
          {/* <Description /> */}
          <Team />
          <Partners />
        </div>
      </div>
    );
  }
}

module.exports = Index;
