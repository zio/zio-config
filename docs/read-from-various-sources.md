---
id: read-from-various-sources
title: "Config Providers"
sidebar_label: "Config Providers"
---

A `ConfigProvider` is the source of your settings. You build one from a string, a file, a map or a remote store, and then load a config description from it. The description stays the same whichever source you pick.

## Choosing a source

| Source | Module | Import | Build a provider with |
| --- | --- | --- | --- |
| HOCON and JSON | `zio-config-typesafe` | `zio.config.typesafe._` | `fromHoconString`, `fromHoconFile`, `fromHoconFilePath`, `fromResourcePath` |
| YAML | `zio-config-yaml` | `zio.config.yaml._` | `fromYamlString`, `fromYamlFile`, `fromYamlPath`, `fromYamlReader` |
| TOML | `zio-config-toml` | `zio.config.toml._` | `fromTomlString`, `fromTomlFile`, `fromTomlPath`, `fromTomlReader` |
| XML (experimental) | `zio-config-xml` | `zio.config.xml.experimental._` | `XmlConfigProvider.fromXmlString` |
| Map | `zio-config` (core) | none | `ConfigProvider.fromMap` |
| AWS Parameter Store | `zio-config-aws` or `zio-config-zio-aws` | `zio.config.aws.parameterstore._` | `fromParameterStore`. See [AWS Parameter Store](./aws-parameter-store.md). |

Each `from...` method has a `...ZIO` variant that returns a `Task[ConfigProvider]`. Use it when building the provider can fail, for example when reading a file.

Every provider can also take a flag, `enableCommaSeparatedValueAsList`, which splits a string value on commas when it is read as a list. It defaults to `false`.

The examples below use these shared definitions:

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig

final case class Settings(host: String, port: Int, region: Option[String])

val settings: Config[Settings] = deriveConfig[Settings]
```

## HOCON and JSON

Add `zio-config-typesafe`. HOCON is a superset of JSON, so the same provider reads both. A HOCON document can use `=` or `:`, and newlines can separate entries.

```scala mdoc:silent
import zio.config.typesafe._

val fromHocon: ConfigProvider =
  ConfigProvider.fromHoconString(
    """
    host = "localhost"
    port = 8080
    region = "us-east"
    """
  )

val loadedFromHocon: IO[Config.Error, Settings] = fromHocon.load(settings)
```

A JSON document is also read by the HOCON provider:

```scala mdoc:silent
val fromJson: ConfigProvider =
  ConfigProvider.fromHoconString(
    """
    {
      "host": "localhost",
      "port": 8080
    }
    """
  )
```

To read a file, use `fromHoconFile` with a `java.io.File`, or `fromHoconFilePath` with a path string. To read from the classpath, use `fromResourcePath`:

```scala mdoc:compile-only
import java.io.File

val fromFile: ConfigProvider = ConfigProvider.fromHoconFile(new File("application.conf"))

val fromResource: ConfigProvider = ConfigProvider.fromResourcePath()
```

## YAML

Add `zio-config-yaml`:

```scala mdoc:compile-only
import zio.config.yaml._

val fromYaml: ConfigProvider =
  ConfigProvider.fromYamlString(
    """
    host: localhost
    port: 8080
    region: us-east
    """
  )
```

`fromYamlFile` and `fromYamlPath` read the same format from disk.

## TOML

Add `zio-config-toml`:

```scala mdoc:compile-only
import zio.config.toml._

val fromToml: ConfigProvider =
  ConfigProvider.fromTomlString(
    """
    host = "localhost"
    port = 8080
    region = "us-east"
    """
  )

val fromTomlFileProvider: ConfigProvider = ConfigProvider.fromTomlFile(new java.io.File("application.toml"))

val fromTomlPathProvider: ConfigProvider = ConfigProvider.fromTomlPath(java.nio.file.Path.of("application.toml"))
```

TOML tables become nested keys. The keys under a `[server]` table are read with `nested("server")`. TOML arrays are read with `Config.listOf`:

```scala mdoc:silent
final case class TomlServer(host: String, ports: List[Int])

val serverConfig: Config[TomlServer] =
  (Config.string("host") zip Config.listOf("ports", Config.int))
    .to[TomlServer]
    .nested("server")
```

```scala mdoc:compile-only
import zio.config.toml._

val serverToml: ConfigProvider =
  ConfigProvider.fromTomlString(
    """
    [server]
    host = "localhost"
    ports = [8080, 8443]
    """
  )

val server: IO[Config.Error, TomlServer] = serverToml.load(serverConfig)
```

By default, a comma inside a string value is not split. Pass `enableCommaSeparatedValueAsList = true` to split such values when they are read as lists:

```scala mdoc:compile-only
import zio.config.toml._

val csvAsList: ConfigProvider =
  ConfigProvider.fromTomlString(
    """
    ports = "8080,8443"
    """,
    enableCommaSeparatedValueAsList = true
  )
```

## XML (experimental)

The XML module is experimental. It does not support XML comments, and it has not been tested with complex data types. Lists are not supported.

Each attribute, and the text of each element, becomes a key. The key path starts at the root tag, so `<config><aws region="us-east"/></config>` produces the key `config.aws.region`. An attribute and a child element with the same name map to the same key. Repeated sibling elements with the same name produce the same key, so the last one wins.

Use `XmlConfigProvider.fromXmlString`:

```scala mdoc:compile-only
import zio.config.xml.experimental._
import zio.Config

final case class Aws(region: String, account: String)

object Aws {
  val config: Config[Aws] = (Config.string("region") zip Config.string("account")).to[Aws]
}

final case class Database(port: Int, url: String)

object Database {
  val config: Config[Database] = (Config.int("port") zip Config.string("url")).to[Database]
}

final case class Configuration(aws: Aws, database: Database)

object Configuration {
  val config: Config[Configuration] =
    (Aws.config.nested("aws") zip Database.config.nested("database")).to[Configuration].nested("config")
}

val xml: String =
  """<config>
    |  <aws region="us-east" account="personal"></aws>
    |  <database port="123" url="some url"></database>
    |</config>""".stripMargin

val parsed: IO[Config.Error, Configuration] =
  XmlConfigProvider.fromXmlString(xml).load(Configuration.config)
```

The package also adds `ConfigProvider.fromYamlString`, which reads XML too. The name is wrong, and the method may be renamed in a later release. Use `XmlConfigProvider.fromXmlString`.

## Maps

`ConfigProvider.fromMap` reads a `Map[String, String]`. It is the simplest provider, and it is useful in tests:

```scala mdoc:silent
val fromMap: ConfigProvider =
  ConfigProvider.fromMap(Map("host" -> "localhost", "port" -> "8080"))

val loadedFromMap: IO[Config.Error, Settings] = fromMap.load(settings)
```

Nested keys in a map use a dot as the separator by default. Pass `pathDelim` to change it.

## Combining sources

`orElse` on a provider tries the first source, and uses the second one for any key the first one does not have:

```scala mdoc:compile-only
import zio.config.typesafe._

val layered: ConfigProvider =
  ConfigProvider
    .fromHoconString("host = \"dev.example.com\"")
    .orElse(ConfigProvider.fromHoconString("host = \"localhost\"\nport = 8080"))
```

Loading `layered` with `Settings` reads `host` from the first source and `port` from the second.

## Lists from flat keys

A flat source such as a map can hold lists as indexed keys, such as `servers[0]` and `servers[1]`. Describe the list with `Config.listOf` and the prefix of the keys:

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

For nested lists in a structured format, HOCON, YAML and TOML are easier to read than flat keys.

## Next steps

- [Config descriptors](./config-descriptors.md) explains how to describe the keys a provider reads.
- [Errors](./errors.md) explains the errors a provider can return.
- [Testing config](./how-to-test-config.md) shows how to use `fromMap` in tests.
