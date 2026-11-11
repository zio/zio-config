---
id: index
title: "Introduction"
sidebar_label: "Introduction"
---

[ZIO Config](https://zio.dev/zio-config/) is a ZIO library for reading application configuration. It extends the core `Config` type from ZIO. You describe the settings your program needs once, as a value of type `Config[A]`, and then load that description from any source: HOCON, YAML, TOML, XML, environment variables, maps or AWS Parameter Store.

[![Production Ready](https://img.shields.io/badge/Project%20Stage-Production%20Ready-brightgreen.svg)](https://github.com/zio/zio/wiki/Project-Stages)
![CI Badge](https://github.com/zio/zio-config/workflows/CI/badge.svg)
[![Maven Central](https://maven-badges.sml.io/sonatype-central/dev.zio/zio-config_3/badge.svg)](https://maven-badges.sml.io/sonatype-central/dev.zio/zio-config_3)
[![javadoc](https://javadoc.io/badge2/dev.zio/zio-config-docs_3/javadoc.svg)](https://javadoc.io/doc/dev.zio/zio-config-docs_3)
[![ZIO Config](https://img.shields.io/github/stars/zio/zio-config?style=social)](https://github.com/zio/zio-config)

## Key features

- **Many sources.** Read flat or nested settings from HOCON, JSON, YAML, TOML, XML, maps and more. See [Config providers](./read-from-various-sources.md).
- **Automatic derivation.** Build a config description from a case class or sealed trait with one call. See [Derivation](./automatic-derivation-of-config.md).
- **Validation with refined types.** Put predicates such as "non-empty" or "at least 1024" in the type. See [Validation](./automatic-validations.md).
- **Documentation generation.** Produce a table of every setting, its format and its description. See [Documentation and reporting](./documentation-and-reporting.md).
- **Error accumulation.** Report every problem in one go, not only the first. See [Errors](./errors.md).
- **Integrations.** Works with enumeratum, cats, scalaz, pureconfig migration and AWS. See [Integrations](./integrations.md).

## Where to start

1. [Getting Started](./getting-started.md) adds the dependencies and loads a first config.
2. [Config descriptors](./config-descriptors.md) explains the building blocks.
3. The [Tutorial](./tutorial-hocon-and-env.md) builds a realistic app configuration.
4. The [How-to Guides](./how-to-read-secrets.md) show how to solve specific tasks.

## What's new

If you upgrade from 4.0.6 or earlier, read [What's New Since 4.0.6](./whats-new-since-4-0-6.md) first.

## Examples

Many complete examples are in the [examples module](https://github.com/zio/zio-config/tree/master/examples/shared/src/main/scala/zio/config/examples) of the repository.
