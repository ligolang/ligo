import Config from "Config";
import NetworkAllLogoImg from "./NetworkIcon";

const rawNetworks = Config.networks;

const networks = rawNetworks.map((v) => {
  const newV = {
    id: v.id,
    group: v.group,
    name: v.name,
    fullName: v.fullName,
    icon: "fas fa-globe",
    notification: `Switched to <b>${v.fullName}</b>.`,
    url: v.url,
    explorerUrl: v.explorerUrl,
    type: v.type,
    chainId: `${v.name}+${v.url}`,
    symbol: v.symbol,
    logoIcon:
      v.icon === "tezosmain"
        ? NetworkAllLogoImg.tezosmain
        : v.icon === "dpfmain"
        ? NetworkAllLogoImg.dpfmain
        : undefined,
  };
  return newV;
});

export default networks;

export const customNetworks = [
  {
    id: "custom",
    group: "Others",
    name: "Custom",
    fullName: "Custom Network",
    icon: "fas fa-edit",
    notification: "Switched to <b>Custom</b> network.",
    url: "",
    symbol: "ETH",
  },
];
