const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO Config",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        "dive-into-zio-config",
        "manual-creation-of-config-descriptor",
        "automatic-derivation-of-config-descriptor",
        "read-from-various-sources",
        "config-descriptor-usage",
        "automatic-validations",
        "resources"
      ]
    }
  ]
};

module.exports = sidebars;