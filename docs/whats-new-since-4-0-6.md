---
id: whats-new-since-4-0-6
title: "What's New Since 4.0.6"
sidebar_label: "What's New"
---

This page lists the user-visible changes made after the 4.0.6 release, newest first. Read it when you upgrade from 4.0.6 or earlier to a later 4.x version.

## 4.1.2

Version 4.1.2 contains dependency updates only. It updates tomlj to 2.1.1, Typesafe Config to 1.4.10 and snakeyaml-engine to 3.2, and it updates the build tooling. The ZIO Config API does not change in this version. Version 4.1.1 is tagged on the same commit as 4.1.0 and has no changes of its own.

## 4.1.0

Version 4.1.0 fixes the cats module under Scala 3.9 and moves the Scala 3 build to the 3.9.0 compiler. It also updates snakeyaml-engine to 3.1.1. The project's CI no longer runs on JDK 11 and now runs on JDK 25.

## 4.0.8

Version 4.0.8 adds the `zio-config-toml` module, which reads TOML files and strings. The module provides `ConfigProvider.fromTomlString`, `ConfigProvider.fromTomlFile`, `ConfigProvider.fromTomlPath` and `ConfigProvider.fromTomlReader`, plus `ZIO` variants of each. It also updates Typesafe Config to 1.4.9. See [TOML](./toml-provider.md).

## 4.0.7

Version 4.0.7 adds the `@kebabCaseLegacy` annotation for derived configs. It applies the older kebab-case rule, which inserts a `-` only between a lowercase letter and an uppercase letter. It differs from `@kebabCase` where a digit is followed by an uppercase letter: `version2Name` becomes `version2name` with `@kebabCaseLegacy` and `version2-name` with `@kebabCase`. See [Key Case, Prefixes and Suffixes](./automatic-derivation-of-config.md#key-case-prefixes-and-suffixes).
