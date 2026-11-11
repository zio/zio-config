const sidebars = {
  sidebar: [
    {
      type: "doc",
      id: "index",
      label: "Introduction"
    },
    {
      type: "doc",
      id: "getting-started",
      label: "Getting Started"
    },
    {
      type: "category",
      label: "Reference",
      collapsed: false,
      items: [
        "config-descriptors",
        "read-from-various-sources",
        "automatic-derivation-of-config",
        "automatic-validations",
        "documentation-and-reporting",
        "errors",
        "integrations",
        "aws-parameter-store",
        "whats-new-since-4-0-6",
        "resources"
      ]
    },
    {
      type: "category",
      label: "Tutorials",
      collapsed: false,
      items: ["tutorial-hocon-and-env"]
    },
    {
      type: "category",
      label: "How-to Guides",
      collapsed: false,
      items: [
        "how-to-read-secrets",
        "how-to-override-per-environment",
        "how-to-test-config",
        "how-to-migrate-from-pureconfig"
      ]
    }
  ]
};

module.exports = sidebars;
