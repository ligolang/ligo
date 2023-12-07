import { useHistory, useLocation } from "@docusaurus/router";
import useBaseUrl from "@docusaurus/useBaseUrl";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Image from "@site/src/components/Image";
import ScrollToAnchor from "@site/src/components/ScrollToAnchor";
import SyntaxContext from "@theme/Syntax/SyntaxContext";
import React, { useEffect, useState } from "react";
import { useCookies } from "react-cookie";
import CookieConsent, { Cookies } from "react-cookie-consent";
import { initGA } from "../utils/analytics";

const USER_TIME_ZONE = Intl.DateTimeFormat().resolvedOptions().timeZone;
const COOKIE_CONSENT_NAME = "cookie_consent";
const isUserInEurope = () => USER_TIME_ZONE.toLocaleLowerCase().startsWith("europe");

const VALID_SYNTAXES = ["jsligo", "cameligo", "pascaligo"];
const DEFAULT_SYNTAX = "jsligo";
const LOCAL_STORAGE_SYNTAX_KEY = "syntax";

const isSyntaxValid = (syntax) => VALID_SYNTAXES.includes(syntax);
const extractQueryString = (searchParams) => new URLSearchParams(searchParams).get("lang");
const isLangOnQueryString = (queryString) => Boolean(queryString);

// Default implementation, that you can customize
export default function Root({ children }) {
  const location = useLocation();
  const { search } = location;
  const history = useHistory();
  const { siteConfig } = useDocusaurusContext();
  const [_, setCookie] = useCookies([COOKIE_CONSENT_NAME]);

  const [syntax, setSyntax] = useState(() => {
    if (typeof window === "undefined") return "jsligo";
    // Initialize syntax based on query-string, local-storage or default value
    const syntaxFromLocalStorage = localStorage.getItem(LOCAL_STORAGE_SYNTAX_KEY);
    const langFromQueryString = extractQueryString(search.toLowerCase());
    if (isSyntaxValid(langFromQueryString)) {
      return langFromQueryString;
    }
    if (isSyntaxValid(syntaxFromLocalStorage)) {
      return syntaxFromLocalStorage;
    }
    return DEFAULT_SYNTAX;
  });

  useEffect(() => {
    const lang = extractQueryString(search.toLowerCase());
    // Make lang query string always in the URL (useful to share links)
    if (!isLangOnQueryString(lang)) {
      history.replace({
        search: `?lang=${syntax}`,
      });
    } else {
      setSyntax(lang);
      localStorage.setItem("syntax", lang);
    }
  }, [location.pathname, location.search]);

  const handleAcceptCookie = () => {
    if (siteConfig.customFields.REACT_APP_GOOGLE_ANALYTICS_ID) {
      initGA(siteConfig.customFields.REACT_APP_GOOGLE_ANALYTICS_ID);
    }
  };
  const handleDeclineCookie = () => {
    //remove google analytics cookies
    Cookies.remove("_ga");
    Cookies.remove("_gat");
    Cookies.remove("_gid");
  };

  useEffect(() => {
    // we set a true consent cookie for users not concerned by GPDR
    if (!isUserInEurope()) {
      const dateIn90Days = new Date();
      dateIn90Days.setDate(dateIn90Days.getDate() + 90);
      setCookie(COOKIE_CONSENT_NAME, true, {
        expires: dateIn90Days,
      });
    }
    // uncomment to initialize cookies if user consent at app load
    // const isConsent = getCookieConsentValue(COOKIE_CONSENT_NAME);
    // if (isConsent === "true") {
    handleAcceptCookie();
    // }
  }, []);

  return (
    <>
      {isUserInEurope() && (
        <CookieConsent
          expires={7}
          buttonText="OK!"
          declineButtonText="No, thanks"
          cookieName={COOKIE_CONSENT_NAME}
          enableDeclineButton
          setDeclineCookie
          disableStyles={true}
          onAccept={handleAcceptCookie}
          onDecline={handleDeclineCookie}
          containerClasses="cookies__container"
          overlayClasses="cookies__overlay"
          contentClasses="cookies__content"
          buttonClasses="cookies__button"
          declineButtonClasses="cookies__button-decline"
          buttonWrapperClasses="cookies__button-wrapper"
        >
          <Image
            src={useBaseUrl("img/cookies.svg")}
            height={50}
            width={50}
            alt={"Ligo likes cookies"}
          />
          <p className="cookies__content-description">
            We use cookies to monitor our website and improve your preferred pages. <br />
            Help us provide a good navigation experience by accepting them.
          </p>
        </CookieConsent>
      )}
      <ScrollToAnchor />
      <SyntaxContext.Provider value={{ syntax, setSyntax }}>{children}</SyntaxContext.Provider>
    </>
  );
}
