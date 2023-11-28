import { translate } from "@docusaurus/Translate";
import { PageMetadata } from "@docusaurus/theme-common";
import Layout from "@theme/Layout";
import React from "react";
import Image from "../components/Image";
export default function NotFound() {
  return (
    <>
      <PageMetadata
        title={translate({
          id: "theme.NotFound.title",
          message: "Page Not Found",
        })}
      />
      <Layout>
        <div id="pageNotFoundPage" className="centered">
          <div id="mural">
            <Image className="muralPolygon1" src={`/img/404-mural/polygon1.svg`} />
            <Image className="muralPolygon2" src={`/img/404-mural/polygon2.svg`} />
            <Image className="muralPolygon3" src={`/img/404-mural/polygon3.svg`} />
            <Image className="muralPolygon4" src={`/img/404-mural/polygon4.svg`} />
            <Image className="muralPolygon5" src={`/img/404-mural/polygon5.svg`} />
          </div>
          <div id="message">
            <h1 className="title">404</h1>
            <p className="lookingForSomething">You're looking for something that doesn't exist</p>
            <p className="discoveriesHappen">
              Please contact the owner of the site that linked you to the original URL and let them
              know their link is broken.
            </p>
          </div>
        </div>
      </Layout>
    </>
  );
}
