/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');

class Footer extends React.Component {
  docUrl(doc, language) {
    const baseUrl = this.props.config.baseUrl;
    const docsUrl = this.props.config.docsUrl;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ''}`;
    const langPart = `${language ? `${language}/` : ''}`;
    return `${baseUrl}${docsPart}${langPart}${doc}`;
  }

  pageUrl(doc, language) {
    const baseUrl = this.props.config.baseUrl;
    return baseUrl + (language ? `${language}/` : '') + doc;
  }

  render() {
    return (
      <footer className="nav-footer" id="footer">
        <section className="footer-wrapper">
          <div className="sitemap">
            <div>
              <h5>Docs</h5>
              <a href={this.docUrl('next/intro/installation')}>
                Installation
              </a>
              <a href={this.docUrl('next/api/cli-commands.html')}>
                CLI Commands
              </a>
              <a href={this.docUrl('next/contributors/origin.html')}>
                Contribute
              </a>
              <a href="/odoc">
                Api Documentation
              </a>
            </div>
            <div>
              <h5>Community</h5>
              <a
                href="https://tezos.stackexchange.com/questions/tagged/ligo"
                target="_blank"
                rel="noreferrer noopener">
                Tezos Stack Exchange
              </a>
              <a
                href="https://discord.gg/9rhYaEt"
                target="_blank"
                rel="noreferrer noopener">
                Discord
              </a>
            </div>
            <div>
              <h5>More</h5>
              <a href={`${this.props.config.baseUrl}blog`}>Blog</a>
              <a href={this.docUrl('tutorials/get-started/tezos-taco-shop-smart-contract.html')}>Tutorials</a>
              <a href={`${this.props.config.repoUrl}`}>Gitlab</a>
            </div>
          </div>

          <div className="copyright">
            {this.props.config.copyright}
          </div>
        </section>

        {/* Load the DM Sans font, there's most likely a more appropriate place to load it in docusaurus than here */}
        <link href="https://fonts.googleapis.com/css?family=DM+Sans:400,400i,500,500i,700,700i&display=swap&subset=latin-ext" rel="stylesheet"></link>

      </footer>
    );
  }
}

module.exports = Footer;
