import React from 'react';
import {translate} from '@docusaurus/Translate';
import {PageMetadata} from '@docusaurus/theme-common';
import Layout from '@theme/Layout';
export default function NotFound() {
  return (
    <>
      <PageMetadata
        title={translate({
          id: 'theme.NotFound.title',
          message: 'Page Not Found',
        })}
      />
      <Layout>
        <div id="pageNotFoundPage" className="centered">
          <div id="mural">
            <img
              className="muralPolygon1"
              src={`/img/404-mural/polygon1.svg`}
            />
            <img
              className="muralPolygon2"
              src={`/img/404-mural/polygon2.svg`}
            />
            <img
              className="muralPolygon3"
              src={`/img/404-mural/polygon3.svg`}
            />
            <img
              className="muralPolygon4"
              src={`/img/404-mural/polygon4.svg`}
            />
            <img
              className="muralPolygon5"
              src={`/img/404-mural/polygon5.svg`}
            />
          </div>
          <div id="message">
            <div className="title">404</div>
            <div className="lookingForSomething">
              You're looking for something that doesn't exist
            </div>
            <div className="discoveriesHappen">
              Please contact the owner of the site that linked you to the
              original URL and let them know their link is broken.
            </div>
          </div>
        </div>
      </Layout>
    </>
  );
}
