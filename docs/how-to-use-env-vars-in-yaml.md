---
id: how-to-use-env-vars-in-yaml
title: "How to Use Environment Variables in a YAML File"
sidebar_label: "Use Environment Variables in YAML"
---

A YAML configuration file often needs a few values that differ per machine, such as a port, or that must not be committed, such as a password. The YAML provider can take those values from environment variables while it reads the file, so one file serves every environment.

## Goal

Write one `application.yaml` that has defaults for development, takes overrides from the environment, and refuses to start when a required secret is missing.

## Step 1: Add the YAML Module

Add the YAML provider and derivation:

```scala
// YAML source (JVM)
libraryDependencies += "dev.zio" %% "zio-config-yaml"     % "@VERSION@"

// Automatic derivation of config descriptors (JVM)
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "@VERSION@"
```

## Step 2: Write the File With Substitutions

Use `${NAME:-default}` for a value with a default, and `${NAME:?message}` for a value that must be set. Keep each expression as the whole, unquoted value, and keep defaults and messages to letters, digits and `_`; the provider leaves anything else as plain text. The [YAML reference](./yaml-provider.md#environment-variables-in-yaml) lists the rules:

```yaml
server:
  host: ${APP_HOST:-localhost}
  port: ${APP_PORT:-8080}
database:
  url: ${DATABASE_URL:-jdbc_dev_placeholder}
  password: ${DATABASE_PASSWORD:?DATABASE_PASSWORD_missing}
```

A value such as a URL usually contains characters that are not allowed in a default. Give it a placeholder default, as above, or require it with `?`.

## Step 3: Describe the Settings

Derive the description from case classes that match the file:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.yaml._

final case class Server(host: String, port: Int)
final case class Database(url: String, password: Config.Secret)
final case class AppConfig(server: Server, database: Database)

val appConfig: Config[AppConfig] = deriveConfig[AppConfig]
```

## Step 4: Load the File as an Effect

Use a `...ZIO` constructor, so that a missing file or a missing required variable fails the effect instead of throwing. The substitution happens when the provider is built, so the `?` check fails before any key is read:

```scala mdoc:compile-only
import java.nio.file.Paths

val loaded: Task[AppConfig] =
  for {
    provider <- ConfigProvider.fromYamlPathZIO(Paths.get("application.yaml"))
    config   <- provider.load(appConfig)
  } yield config
```

To use the file for the whole program, build the provider in the `bootstrap` layer and read the settings with `ZIO.config`:

```scala mdoc:compile-only
import java.nio.file.Paths

object Main extends ZIOAppDefault {
  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    ZLayer.fromZIO(ConfigProvider.fromYamlPathZIO(Paths.get("application.yaml"))).flatMap(provider =>
      Runtime.setConfigProvider(provider.get)
    )

  def run = ZIO.config(appConfig).flatMap(config => Console.printLine(s"Listening on ${config.server.port}"))
}
```

## Step 5: Check the Defaults

With none of the variables set, except the required password, the file loads with its defaults. In this check the password is written directly into the YAML text, so the example does not depend on the environment of the machine that runs it:

```scala mdoc
val yaml: String =
  """
  server:
    host: ${ZIO_CONFIG_DOCS_UNSET_HOST:-localhost}
    port: ${ZIO_CONFIG_DOCS_UNSET_PORT:-8080}
  database:
    url: ${ZIO_CONFIG_DOCS_UNSET_URL:-jdbc_dev_placeholder}
    password: not-a-real-secret
  """

Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(ConfigProvider.fromYamlStringZIO(yaml).flatMap(_.load(appConfig))).getOrThrow()
}
```

Then run the program with an override, for example `APP_PORT=9090 DATABASE_PASSWORD=... sbt run`, and check that the port changes. Run it once without `DATABASE_PASSWORD` and check that it stops with `DATABASE_PASSWORD_missing`.

## Notes

- Substitution is a feature of the YAML provider. In HOCON, use `${?NAME}`; see [HOCON and JSON](./hocon-provider.md#substitutions-and-environment-variables).
- To override any key without editing the file, layer the environment over the YAML provider instead: `ConfigProvider.envProvider.orElse(yamlProvider)`. See [How to override settings per environment](./how-to-override-per-environment.md).
- `Config.Secret` keeps the password out of logs; see [How to read secrets](./how-to-read-secrets.md).
