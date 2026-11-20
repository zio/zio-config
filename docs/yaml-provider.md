---
id: yaml-provider
title: "YAML"
sidebar_label: "YAML"
---

The `zio-config-yaml` module reads YAML with [snakeyaml-engine](https://bitbucket.org/snakeyaml/snakeyaml-engine) and turns it into a `ConfigProvider`. It also substitutes environment variables written as `${NAME}` inside the YAML text.

To add the module:

```scala
// YAML source (JVM)
libraryDependencies += "dev.zio" %% "zio-config-yaml" % "@VERSION@"
```

## Building a Provider

`import zio.config.yaml._` adds these constructors to `ConfigProvider`. The same functions, except `fromYamlRepr`, are on the `YamlConfigProvider` object:

| Constructor                                         | Reads                                                                 |
| --------------------------------------------------- | --------------------------------------------------------------------- |
| `ConfigProvider.fromYamlString(yamlString)`         | A YAML string                                                         |
| `ConfigProvider.fromYamlFile(file)`                 | A `java.io.File`                                                      |
| `ConfigProvider.fromYamlPath(path)`                 | A `java.nio.file.Path`                                                |
| `ConfigProvider.fromYamlReader(reader)`             | A `java.io.Reader`, for example a classpath resource                  |
| `ConfigProvider.fromYamlRepr(repr)(loadYaml)`       | Any value, turned into parsed YAML by your `loadYaml` function        |

Each constructor parses its input when it is called, and throws if the input is missing or malformed. `fromYamlString`, `fromYamlFile`, `fromYamlPath` and `fromYamlReader` have `...ZIO` variants, such as `ConfigProvider.fromYamlFileZIO`, that return a `Task[ConfigProvider]` and fail the task instead. `ConfigProvider.fromYamlReader` reads from any `java.io.Reader`, for example one opened on a classpath resource; close the reader yourself when the provider is built. Every constructor also takes `enableCommaSeparatedValueAsList` (default `false`), which splits string values on commas when they are read as lists.

The examples on this page share these definitions and a helper, `fromYaml`, which loads a description from a YAML string:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.yaml._

def fromYaml[A](config: Config[A], yaml: String): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ConfigProvider.fromYamlString(yaml).load(config).either).getOrThrow()
  }

final case class Database(url: String, poolSize: Int)
final case class AppConfig(name: String, database: Database, replicas: List[String])
```

## Mappings and Sequences

A YAML mapping becomes a nested key, and a sequence becomes a list:

```scala mdoc
fromYaml(deriveConfig[AppConfig], """
name: orders
database:
  url: postgres://localhost/orders
  poolSize: 10
replicas:
  - eu-1
  - eu-2
""")
```

A sequence of mappings is a list of case classes, and an empty sequence `[]` is an empty list:

```scala mdoc
fromYaml(Config.listOf("databases", deriveConfig[Database]), """
databases:
  - url: postgres://a/app
    poolSize: 5
  - url: postgres://b/app
    poolSize: 7
""")

fromYaml(Config.listOf("databases", deriveConfig[Database]), "databases: []")
```

A key with no value, or with `null` or `~`, is treated as missing:

```scala mdoc
fromYaml(Config.string("region").optional, "region: null")
```

## Environment Variables in YAML

While parsing, the provider replaces a value written as `${NAME}` with the value of the environment variable `NAME`. Three forms control what happens when the variable is not set:

| Form                     | Variable set | Variable not set                                                   |
| ------------------------ | ------------ | ------------------------------------------------------------------ |
| `${NAME}`                | Its value    | The text `${NAME}` is kept as is                                   |
| `${NAME:-default}`       | Its value    | `default`                                                          |
| `${NAME:?message}`       | Its value    | Building the provider fails with an error that contains `message`  |

The `-` and `?` forms without a colon (`${NAME-default}`, `${NAME?message}`) behave the same way.

The substitution follows the rules of snakeyaml-engine, which are strict:

- The expression must be the whole value, with no other text around it: `port: ${PORT:-8080}` works, `url: http://${HOST}:8080` is kept as text.
- The value must be a plain scalar; a quoted value such as `"${PORT}"` is kept as text.
- The default and the message may contain only letters, digits and `_`. `${HOST:-localhost}` works; `${HOST:-0.0.0.0}` or a message with spaces is kept as text.

In this example the variables are not set. The first two values use their defaults, and the third one, whose default contains dots, is not substituted:

```scala mdoc
final case class Http(host: String, port: Int, bind: String)

fromYaml(deriveConfig[Http], """
host: ${ZIO_CONFIG_DOCS_UNSET_HOST:-localhost}
port: ${ZIO_CONFIG_DOCS_UNSET_PORT:-8080}
bind: ${ZIO_CONFIG_DOCS_UNSET_BIND:-0.0.0.0}
""")
```

Use the `?` form for values that must come from the environment, such as passwords. With a `...ZIO` constructor, the missing variable fails the task that builds the provider:

```scala mdoc:silent
val needsPassword: Task[ConfigProvider] =
  ConfigProvider.fromYamlStringZIO("password: ${ZIO_CONFIG_DOCS_UNSET_PASSWORD:?required}")
```

Running the task fails, and the error message is the text after `?`:

```scala mdoc
Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(needsPassword.either.map(_.left.map(_.getMessage))).getOrThrow()
}
```

## Bringing Your Own Parser

`ConfigProvider.fromYamlRepr` takes a value of any type and a function that turns it into parsed YAML: Java maps, lists, strings, numbers and booleans, as snakeyaml-engine produces them. Use it when the YAML is already parsed, or comes from another library:

```scala mdoc
import scala.jdk.CollectionConverters._

val parsed: java.util.Map[String, AnyRef] =
  Map[String, AnyRef]("url" -> "postgres://repr/app", "poolSize" -> Integer.valueOf(3)).asJava

Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(
    ConfigProvider.fromYamlRepr(parsed)(identity).load(deriveConfig[Database])
  ).getOrThrow()
}
```

## Next Steps

- [Config providers](./read-from-various-sources.md) compares every source.
- [How to use environment variables in YAML](./how-to-use-env-vars-in-yaml.md) shows a complete setup.
