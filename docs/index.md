---
id: index
title: "Getting Started with ZIO Config"
sidebar_label: "Getting Started"
---

[ZIO Config](https://zio.dev/zio-config/) is a ZIO-based library and act as an extension to core library ZIO's `Config` language.

@PROJECT_BADGES@

Let's enumerate some key features of this library:

- **Support for Various Sources** — It can read flat or nested configurations. Thanks to `IndexedFlat`.
- **Automatic Document Generation** — It can auto-generate documentation of configurations.
- **Automatic Derivation** — It has built-in support for automatic derivation of readers and writers for case classes and sealed traits.
- **Type-level Constraints and Automatic Validation** — because it supports _Refined_ types, we can write type-level predicates which constrain the set of values described for data types.
- **Descriptive Errors** — It accumulates all errors and reports all of them to the user rather than failing fast.
- **Integrations** — Integrations with a variety of libraries


If you are only interested in automatic derivation of configuration, find the details [here](https://zio.dev/zio-config/automatic-derivation-of-config)

## Installation

In order to use this library, we need to add the following line in our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-config" % "@VERSION@" 
```

# Quick Start

Let's add these four lines to our `build.sbt` file as we are using these modules in our examples:

```scala
libraryDependencies += "dev.zio" %% "zio-config"          % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-config-typesafe" % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-config-refined"  % "@VERSION@"
```

There are many examples in [here](https://github.com/zio/zio-config/tree/master/examples/shared/src/main/scala/zio/config/examples)
