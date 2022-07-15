export default {
  title: "Gas",
  list: [
    {
      name: "gasLimit",
      alias: "gas",
      className: "col-4",
      label: "Gas Limit",
      icon: "fas fa-burn",
      placeholder: "Default: 1,000,000",
      default: "1000000",
    },
    {
      name: "maxPriorityFeePerGas",
      className: "col-4",
      label: "Tip",
      icon: "fas fa-hand-holding-usd",
      placeholder: "max priority fee per gas",
      default: "",
    },
    {
      name: "maxFeePerGas",
      className: "col-4",
      label: "Max Fee",
      icon: "fas fa-dollar-sign",
      placeholder: "max fee per gas",
      default: "",
    },
  ],
};
