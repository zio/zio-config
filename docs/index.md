---
id: index
title: "Getting Started with ZIO Config"
sidebar_label: "Getting Started"
---

[ZIO Config](https://zio.github.io/zio-config/) is a ZIO-based library for loading and parsing configuration sources.

_ZIO Config_ acts an extension to ZIO's `Config` with documentation support, with more complex data-sources, backed by an `IndexedFlat`
Most of the configuration sources are relying on `IndexedFlat`.

More documentation around `IndexedFlat` (an extension of  ZIO's  `ConfigProvider.Flat`)
to handle more complex sources will be provided soon.

@PROJECT_BADGES@

## Introduction

The library aims to have a powerful & purely functional, yet a thin interface to access configuration information inside an application. 
Note that some of the DSL's have been moved to core ZIO now.

The ZIO Config has a lot of features, and it is more than just a config parsing library. Let's enumerate some key features of this library:

- **Support for Various Sources** — It can read/write flat or nested configurations from/to various formats and sources.
- **Automatic Document Generation** — It can auto-generate documentation of configurations. So developers or DevOps engineers know how to configure the application.
- **Automatic Derivation** — It has built-in support for automatic derivation of readers and writers for case classes and sealed traits.
- **Type-level Constraints and Automatic Validation** — because it supports _Refined_ types, we can write type-level predicates which constrain the set of values described for data types.
- **Descriptive Errors** — It accumulates all errors and reports all of them to the user rather than failing fast.

Using a single definition of configuration requirements, which can be derived automatically from your data types, _ZIO Config_ offers a bundle of features for free:

* Read flat or nested config data from any format
* Automatically generate documentation so devs / devops know how to configure the application

If you are only interested in automatic derivation of configuration, find the details [here](http://zio.dev/zio-config/automatic-derivation-of-config-descriptor).

## Installation

In order to use this library, we need to add the following line in our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-config" % "@VERSION@" 
```

There are also some optional dependencies:

```scala
// Optional Dependency with magnolia module (Auto derivation)
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "@VERSION@"

// Optional Dependency with refined module (Integration with refined library)
libraryDependencies += "dev.zio" %% "zio-config-refined" % "@VERSION@"

// Optional Dependency with typesafe module (HOCON/Json source)
libraryDependencies += "dev.zio" %% "zio-config-typesafe" % "@VERSION@"

// Optional Dependency with yaml module (Yaml source)
libraryDependencies += "dev.zio" %% "zio-config-yaml" % "@VERSION@"

// Optional Dependency for a random generation of a config
libraryDependencies += "dev.zio" %% "zio-config-gen" % "@VERSION@"
```

## Example

Let's add these four lines to our `build.sbt` file as we are using these modules in our example:

```scala
libraryDependencies += "dev.zio" %% "zio-config"          % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-config-typesafe" % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-config-refined"  % "@VERSION@"
```


There are many examples in [here](https://github.com/zio/zio-config/tree/master/examples/shared/src/main/scala/zio/config/examples) straight away as well.

Try out _ZIO Config_ quickly in [Scastie](https://scastie.scala-lang.org/WMlkdQeZQvm4yDyZ0pigJA), which comes pre-loaded with an example in scala-3. We try to make sure the scastie-buildsettings are updated with latest version of _ZIO Config_.
