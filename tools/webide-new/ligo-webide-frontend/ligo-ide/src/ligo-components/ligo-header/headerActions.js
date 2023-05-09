import redux from "~/base-components/redux";

export class HeaderActions {
  constructor() {
    this.history = null;
  }

  updateNetwork(networkId) {
    if (this?.history?.location?.pathname?.startsWith("/network") && networkId !== "custom") {
      this.history.push(`/network/${networkId}`);
    }
  }
}

export default new HeaderActions();
