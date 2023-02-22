const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO Config",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        "index",
        "dive-into-zio-config",
        "manual-creation-of-config-descriptor",
        "automatic-derivation-of-config",
        "read-from-various-sources",
        "automatic-validations",
        "resources"
      ]
    }
  ]
};

module.exports = sidebars;