const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO Config",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        "index",
        "defining-config-descriptors",
        "auto-generation-of-config-documentation",
        "integrations",
        "automatic-derivation-of-config",
        "read-from-various-sources",
        "automatic-validations",
        "resources"
      ]
    }
  ]
};

module.exports = sidebars;