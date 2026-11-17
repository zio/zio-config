---
id: index
title: "Introduction"
sidebar_label: "Introduction"
---

[ZIO Config](https://zio.dev/zio-config/) is a ZIO library for reading application configuration. It extends the `Config` and `ConfigProvider` types from ZIO. You describe the settings your program needs once, as a value of type `Config[A]`, and then load that description from any source: HOCON, JSON, YAML, TOML, XML, environment variables, maps or AWS Parameter Store.

@PROJECT_BADGES@

## Key Features

- **Many sources.** Read flat or nested settings from HOCON, JSON, YAML, TOML, XML, maps, the environment and more, and layer them. See [Config providers](./read-from-various-sources.md).
- **Automatic derivation.** Build a config description from a case class or sealed trait with one call. See [Derivation](./automatic-derivation-of-config.md).
- **Validation.** Check values with `Config#validate`, custom parsers or refined types. See [Validation](./automatic-validations.md).
- **Documentation generation.** Produce a table of every setting, its format and its description. See [Documentation generation](./documentation-and-reporting.md).
- **Error accumulation.** Report every problem in one go, not only the first. See [Errors](./errors.md).
- **Integrations.** Works with enumeratum, cats, scalaz, pureconfig and AWS. See [Integrations](./integrations.md).

## Where to Start

1. [Getting Started](./getting-started.md) adds the dependencies and loads a first config.
2. The tutorials teach the concepts step by step: [modelling a configuration with case classes and sealed traits](./tutorial-model-config-with-adts.md), [loading it in a ZIO application](./tutorial-config-in-a-zio-app.md), and [layering HOCON with environment variables](./tutorial-hocon-and-env.md).
3. The how-to guides solve specific tasks, such as [setting the config provider for an application](./how-to-set-the-config-provider.md), [reading secrets](./how-to-read-secrets.md) and [testing configuration](./how-to-test-config.md).
4. The reference pages describe every part of the API, starting with [Config descriptors](./config-descriptors.md).

## Migrating

- From ZIO Config 3.x: [How to migrate from ZIO Config 3.x](./how-to-migrate-from-zio-config-3.md).
- From pureconfig: [How to migrate from pureconfig](./how-to-migrate-from-pureconfig.md).
- From 4.0.6 or earlier 4.x versions: [What's new since 4.0.6](./whats-new-since-4-0-6.md).

## Examples

More complete examples are in the [examples module](https://github.com/zio/zio-config/tree/series/4.x/examples/shared/src/main/scala/zio/config/examples) of the repository.
