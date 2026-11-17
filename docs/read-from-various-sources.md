---
id: read-from-various-sources
title: "Config Providers"
sidebar_label: "Config Providers"
---

A `ConfigProvider` is the source of your settings. You build one from a string, a file, the environment or a remote store, and then load a config description from it. The description stays the same whichever source you pick, so you can move from a map in tests to a HOCON file in production without touching it.

`ConfigProvider` is a ZIO type. ZIO ships providers for maps, environment variables, system properties and command-line arguments; ZIO Config adds one module per file format and one for AWS Parameter Store.

## Choosing a Source

Each row links to the page with the details:

| Source                                         | Module                                  | Import                              | Main constructors                                                               |
| ---------------------------------------------- | --------------------------------------- | ----------------------------------- | ------------------------------------------------------------------------------- |
| [Map, environment, properties, arguments](./zio-core-providers.md) | none (ZIO core)   | none                                | `ConfigProvider.fromMap`, `envProvider`, `propsProvider`, `fromAppArgs`         |
| [HOCON and JSON](./hocon-provider.md)          | `zio-config-typesafe`                   | `zio.config.typesafe._`             | `fromHoconString`, `fromHoconFile`, `fromHoconFilePath`, `fromResourcePath`, `fromTypesafeConfig` |
| [YAML](./yaml-provider.md)                     | `zio-config-yaml`                       | `zio.config.yaml._`                 | `fromYamlString`, `fromYamlFile`, `fromYamlPath`, `fromYamlReader`, `fromYamlRepr` |
| [TOML](./toml-provider.md)                     | `zio-config-toml`                       | `zio.config.toml._`                 | `fromTomlString`, `fromTomlFile`, `fromTomlPath`, `fromTomlReader`              |
| [XML (experimental)](./xml-provider.md)        | `zio-config-xml`                        | `zio.config.xml.experimental._`     | `XmlConfigProvider.fromXmlString`                                               |
| [AWS Parameter Store](./aws-parameter-store.md) | `zio-config-aws` or `zio-config-zio-aws` | `zio.config.aws.parameterstore._`  | `fromParameterStore`                                                            |

The constructors for files and strings in the ZIO Config modules read their input when you call them, and throw if it is malformed or, for YAML and TOML, missing (a missing HOCON file reads as an empty configuration). Most of them have a `...ZIO` variant, such as `ConfigProvider.fromHoconFileZIO`, that returns a `Task[ConfigProvider]` and fails the task instead; use it whenever the input comes from outside the program. `ConfigProvider.fromYamlRepr` and `XmlConfigProvider.fromXmlString` have no such variant.

## A First Example

The same description loads from a HOCON string and from a map:

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

final case class Settings(host: String, port: Int, region: Option[String])

val settings: Config[Settings] = deriveConfig[Settings]

val fromHocon: ConfigProvider =
  ConfigProvider.fromHoconString(
    """
    host = "localhost"
    port = 8080
    region = "us-east"
    """
  )

val fromMap: ConfigProvider =
  ConfigProvider.fromMap(Map("host" -> "localhost", "port" -> "8080"))
```

Loading returns a `ZIO` that fails with a `Config.Error`:

```scala mdoc:silent
val loadedFromHocon: IO[Config.Error, Settings] = fromHocon.load(settings)
val loadedFromMap: IO[Config.Error, Settings]   = fromMap.load(settings)
```

In a ZIO application, you can also set the provider once with `Runtime.setConfigProvider` and read the description anywhere with `ZIO.config(settings)`. See [ZIO core providers](./zio-core-providers.md).

## Lists and Comma-Separated Values

Structured formats have real lists: arrays in HOCON and TOML, sequences in YAML. Flat sources such as maps and environment variables hold lists in one of two ways: as indexed keys, such as `servers[0]` and `servers[1]`, or as one value with separators, such as `servers = "a,b"`.

The providers differ in how they treat a comma inside a value:

| Provider                                         | Splits `"a,b"` into a list by default | How to change it                                    |
| ------------------------------------------------ | ------------------------------------- | --------------------------------------------------- |
| ZIO core: `fromMap`, `fromEnv`, `fromProps`      | Yes                                   | Pass another `seqDelim`                             |
| ZIO Config: HOCON, YAML, TOML                    | No                                    | Pass `enableCommaSeparatedValueAsList = true`       |

Indexed keys work with every flat provider. Each index holds one element, which can be a whole structure:

```scala mdoc:silent
final case class Employee(age: Int, name: String)

val employees: Config[List[Employee]] =
  Config.listOf("employees", deriveConfig[Employee]).nested("department")

val employeeSource: ConfigProvider =
  ConfigProvider.fromMap(
    Map(
      "department.employees[0].age"  -> "10",
      "department.employees[0].name" -> "foo",
      "department.employees[1].age"  -> "11",
      "department.employees[1].name" -> "bar"
    )
  )
```

Loading it gives both employees, in index order:

```scala mdoc
Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(employeeSource.load(employees)).getOrThrow()
}
```

## Combining Sources

`ConfigProvider#orElse` tries the first source and uses the second one for any key the first one does not have. Put the source that should win first:

```scala mdoc:compile-only
val layered: ConfigProvider =
  ConfigProvider
    .fromHoconString("host = \"dev.example.com\"")
    .orElse(ConfigProvider.fromHoconString("host = \"localhost\"\nport = 8080"))
```

Loading `layered` with `Settings` reads `host` from the first source and `port` from the second. [ZIO core providers](./zio-core-providers.md) lists the other combinators, such as `ConfigProvider#nested` and the key-case conversions.

## Next Steps

- [Config descriptors](./config-descriptors.md) explains how to describe the keys a provider reads.
- [Errors](./errors.md) explains the errors a load can return.
- [How to test config](./how-to-test-config.md) shows how to use `fromMap` in tests.
