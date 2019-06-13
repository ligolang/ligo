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
        <section className="sitemap">
          <a className="nav-home"></a>
          <div>
            <h5>Docs</h5>
            <a href={this.docUrl('setup-installation.html', this.props.language)}>
              Installation
            </a>
            <a href={this.docUrl('api-cli-commands.html', this.props.language)}>
              CLI Commands
            </a>
            <a href={this.docUrl('contributors/origin.html', this.props.language)}>
              Contribute
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
            <a href={this.docUrl('tutorials/get-started/tezos-taco-shop-smart-contract.html', this.props.language)}>Tutorials</a>
            <a href={`${this.props.config.repoUrl}`}>Gitlab</a>
          </div>
        </section>

      
        <section className="copyright">
          Website built with <a
            href="https://docusaurus.io/"
            target="_blank"
            rel="noreferrer noopener">
            Docusaurus
          </a> by Facebook.
          <div>Icons made by <a href="https://www.freepik.com/" title="Freepik">Freepik</a> & <a href="https://www.flaticon.com/authors/lucy-g" title="Lucy G">Lucy G</a> from <a href="https://www.flaticon.com/" 			    title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" 			    title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
          {this.props.config.copyright}
        </section>
      </footer>
    );
  }
}

module.exports = Footer;
