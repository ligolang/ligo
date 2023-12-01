import ReactGA from "react-ga4";
import TagManager from "react-gtm-module";

export const initGA = (id: string) => {
  if (process.env.NODE_ENV === "production") {
    ReactGA.initialize(id);
    TagManager.initialize({
      gtmId: id,
    });
  }
};
