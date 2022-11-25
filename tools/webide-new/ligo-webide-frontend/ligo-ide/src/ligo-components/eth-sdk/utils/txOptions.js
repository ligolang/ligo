export default {
  title: "Gas",
  list: [
    {
      name: "gasLimit",
      alias: "gas",
      className: "col-4",
      label: "Gas Limit",
      icon: "fas fa-burn",
      placeholder: "gas limit",
      default: "",
    },
    {
      name: "storageLimit",
      className: "col-4",
      label: "Storage Limit",
      icon: "fas fa-database",
      placeholder: "storage limit",
      default: "",
    },
    {
      name: "fee",
      className: "col-4",
      label: "Fee (suggested)",
      icon: "fas fa-dollar-sign",
      placeholder: "fee",
      default: "",
    },
  ],
};
