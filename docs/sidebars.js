const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO Config",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        "dive-into-zio-config",
        "integrations",
        "automatic-derivation-of-config",
        "read-from-various-sources",
        "aws-parameter-store",
        "automatic-validations",
        "resources"
      ]
    }
  ]
};

module.exports = sidebars;
