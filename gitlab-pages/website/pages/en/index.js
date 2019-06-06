/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');

const CompLibrary = require('../../core/CompLibrary.js');

const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

class HomeSplash extends React.Component {
  render() {
    const {siteConfig, language = ''} = this.props;
    const {baseUrl, docsUrl} = siteConfig;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ''}`;
    const langPart = `${language ? `${language}/` : ''}`;
    const docUrl = doc => `${baseUrl}${docsPart}${langPart}${doc}`;

    const SplashContainer = props => (
      <div className="homeContainer">
        <div className="homeSplashFade">
          <div className="wrapper homeWrapper">
            
          <div className="tabs">
            <div className="nav-tabs">
              <div id="tab-group-3-tab-4" className="nav-link active" data-group="group_3"
                data-tab="tab-group-3-content-4">Pascaligo</div>
              {/* <div id="tab-group-3-tab-5" className="nav-link" data-group="group_3"
                data-tab="tab-group-3-content-5">Camligo</div> */}
            </div>
            <div className="tab-content">
              <div id="tab-group-3-content-4" className="tab-pane active" data-group="group_3" tabIndex="-1">
                <div>
                  <span>
                    <pre><code className="hljs css language-Pascal">// variant defining pseudo multi-entrypoint actions<br />type action is<br />| Increment of int<br />| Decrement of int<br /><br />function add (const a : int ; const b : int) : int is<br />    block {'{ skip }'} with a + b<br /><br />function subtract (const a : int ; const b : int) : int is<br />    block {'{ skip }'} with a - b<br /><br />// real entrypoint that re-routes the flow based on the action provided<br />function main (const p : action ; const s : int) : (list(operation) * int) is<br />  block {'{ skip }'} with ((nil : list(operation)),<br />    case p of<br />    | Increment n -&gt; add(s, n)<br />    | Decrement n -&gt; subtract(s, n)<br />    end)<br /></code></pre>
                  </span>
                </div>
              </div>
              <div id="tab-group-3-content-5" className="tab-pane" data-group="group_3" tabIndex="-1">
                <div>
                  <span>
                    SOON
                  </span>
                </div>
              </div>
            </div>
          </div>

            {props.children}
          </div>
        </div>
      </div>
    );

    const Logo = props => (
      <div className="projectLogo">
        <img src={props.img_src} alt="Project Logo" />
      </div>
    );

    const ProjectTitle = () => (
      <h2 className="projectTitle">
        <small>{siteConfig.tagline}</small>
      </h2>
    );

    const PromoSection = props => (
      <div className="section promoSection">
        <div className="promoRow">
          <div className="pluginRowBlock">{props.children}</div>
        </div>
      </div>
    );

    const Button = props => (
      <div className="pluginWrapper buttonWrapper">
        <a className="button" href={props.href} target={props.target}>
          {props.children}
        </a>
      </div>
    );

    return (
      <SplashContainer>
        <div className="inner">
          <ProjectTitle siteConfig={siteConfig} />
          <PromoSection>
            <Button href={docUrl('setup-installation.html')}>Get Started</Button>
            <Button href={docUrl('contributors/origin.html')}>Contribute</Button>

          </PromoSection>
        </div>
      </SplashContainer>
    );
  }
}

class Index extends React.Component {
  render() {
    const {config: siteConfig, language = ''} = this.props;
    const {baseUrl} = siteConfig;

    const Block = props => (
      <Container
        padding={['bottom', 'top']}
        id={props.id}
        background={props.background}>
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
        style={{textAlign: 'center'}}>
        <h2>Feature Callout</h2>
        <MarkdownBlock>These are features of this project</MarkdownBlock>
      </div>
    );

    const TryOut = () => (
      <Block id="try">
        {[
          {
            content:
              'To make your landing page more attractive, use illustrations! Check out ' +
              '[**unDraw**](https://undraw.co/) which provides you with customizable illustrations which are free to use. ' +
              'The illustrations you see on this page are from unDraw.',
            image: `${baseUrl}img/undraw_code_review.svg`,
            imageAlign: 'left',
            title: 'Wonderful SVG Illustrations',
          },
        ]}
      </Block>
    );

    const Description = () => (
      <Block background="dark">
        {[
          {
            content:
              'This is another description of how this project is useful',
            image: `${baseUrl}img/undraw_note_list.svg`,
            imageAlign: 'right',
            title: 'Description',
          },
        ]}
      </Block>
    );

    const LearnHow = () => (
      <Block background="light">
        {[
          {
            content:
              'Each new Docusaurus project has **randomly-generated** theme colors.',
            image: `${baseUrl}img/undraw_youtube_tutorial.svg`,
            imageAlign: 'right',
            title: 'Randomly Generated Theme Colors',
          },
        ]}
      </Block>
    );

    const Features = () => (
      <div className="features">
        <h1 className="sectionTitle blockTitle">Features</h1>
        <Block layout="fourColumn">
          {[
            {
              content: 'Write in PascaLIGO (pascal-like syntax) or CameLIGO (caml-like syntax). If you know OCaml, you can also add your own syntax.',
              image: `${baseUrl}img/edit.svg`,
              imageAlign: 'top',
              title: 'Syntax Agnostic',
            },
            {
              content: 'Write types, then code, and benefit from the safety coming from type systems.',
              image: `${baseUrl}img/lightning.svg`,
              imageAlign: 'top',
              title: 'Strong Type System',
            },
            {
              content: 'With Granary, you can use LIGO as a lib from NodeJS.',
              image: `${baseUrl}img/puzzle.svg`,
              imageAlign: 'top',
              title: 'Easy Integration',
            }
          ]}
        </Block>
      </div>
    );

    const Roadmap = () => (
      <div className="roadmap">
        <Block background="light" >
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
              imageAlign: 'right',
              title: 'Roadmap',
            },
          ]}
        </Block>
      </div>
    );

    const Partners = () => {
      if ((siteConfig.partners || []).length === 0) {
        return null;
      }

      const showcase = siteConfig.partners
        .filter(user => user.pinned)
        .map(user => (
          <a href={user.infoLink} key={user.infoLink}>
            <img src={user.image} alt={user.caption} title={user.caption} />
          </a>
        ));

      const pageUrl = page => baseUrl + (language ? `${language}/` : '') + page;

      return (
        <div className="productShowcaseSection paddingBottom">
          <h1 className="sectionTitle">Partners</h1>
          <div className="logos">{showcase}</div>
          {/* <div className="more-users">
            <a className="button" href={pageUrl('users.html')}>
              More {siteConfig.title} Users
            </a>
          </div> */}
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
          <a href={user.infoLink} key={user.infoLink}>
            <img src={user.image} alt={user.caption} title={user.caption} />
            <p>{user.caption}</p>
          </a>
        ));

      const pageUrl = page => baseUrl + (language ? `${language}/` : '') + page;

      return (
        <div className="productShowcaseSection paddingBottom team">
          <h1 className="sectionTitle">Team</h1>
          <div className="logos">{showcase}</div>
          {/* <div className="more-users">
            <a className="button" href={pageUrl('users.html')}>
              More {siteConfig.title} Users
            </a>
          </div> */}
        </div>
      );
    };

    return (
      <div>
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
