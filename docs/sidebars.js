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
        {
          type: "category",
          label: "Config Providers",
          link: { type: "doc", id: "read-from-various-sources" },
          collapsed: false,
          items: [
            "zio-core-providers",
            "hocon-provider",
            "yaml-provider",
            "toml-provider",
            "xml-provider",
            "aws-parameter-store"
          ]
        },
        "automatic-derivation-of-config",
        "derive-config",
        "automatic-validations",
        "documentation-and-reporting",
        "errors",
        "whats-new-since-4-0-6",
        "resources"
      ]
    },
    "integrations",
    {
      type: "category",
      label: "Tutorials",
      collapsed: false,
      items: [
        "tutorial-model-config-with-adts",
        "tutorial-config-in-a-zio-app",
        "tutorial-hocon-and-env"
      ]
    },
    {
      type: "category",
      label: "How-to Guides",
      collapsed: false,
      items: [
        "how-to-set-the-config-provider",
        "how-to-override-per-environment",
        "how-to-read-secrets",
        "how-to-read-lists-and-maps-from-flat-sources",
        "how-to-use-env-vars-in-yaml",
        "how-to-read-a-custom-type",
        "how-to-test-config",
        "how-to-generate-config-docs-in-ci",
        "how-to-migrate-from-pureconfig",
        "how-to-migrate-from-zio-config-3"
      ]
    }
  ]
};

module.exports = sidebars;
