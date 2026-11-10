---
id: how-to-override-per-environment
title: "How to Override Settings per Environment"
sidebar_label: "Override per Environment"
---

Most services run in several environments, such as development, staging and production. This guide shows how to keep one set of defaults, one file per environment, and environment variables that win over both.

## Goal

Select the settings for an environment with one variable, `APP_ENV`, and let any single value be overridden from the process environment.

## Step 1: Keep one file per environment

Store each environment's settings as a HOCON string or file. In a project, these would be `config/dev.conf` and `config/prod.conf`. This guide uses strings:

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

final case class Service(host: String, port: Int, databaseUrl: String)

val devSettings: String =
  """
  HOST = "localhost"
  PORT = 8080
  DATABASE_URL = "postgres://localhost:5432/dev"
  """

val prodSettings: String =
  """
  HOST = "0.0.0.0"
  PORT = 80
  DATABASE_URL = "postgres://db.internal:5432/prod"
  """
```

## Step 2: Choose the file from `APP_ENV`

Read `APP_ENV` from the environment and pick the matching settings. Fail with a clear message for an unknown name:

```scala mdoc:silent
def settingsFor(env: String): Either[String, String] =
  env match {
    case "dev"  => Right(devSettings)
    case "prod" => Right(prodSettings)
    case other  => Left(s"unknown APP_ENV: $other")
  }
```

## Step 3: Layer the environment over the file

Put the process environment first, so a single variable can override the file. The file is the second source:

```scala mdoc:silent
def providerFor(env: String): IO[String, ConfigProvider] =
  ZIO.fromEither(settingsFor(env)).map { hocon =>
    ConfigProvider.envProvider.orElse(ConfigProvider.fromHoconString(hocon))
  }

val serviceDescription: Config[Service] = deriveConfig[Service]
```

Load the service with the provider for the selected environment:

```scala mdoc:compile-only
val load: IO[Any, Service] =
  for {
    env      <- ZIO.succeed(sys.env.getOrElse("APP_ENV", "dev"))
    provider <- providerFor(env)
    service  <- provider.load(serviceDescription).mapError(_.toString)
  } yield service
```

## Step 4: Override one value

Run the program with the file's values, then with one value changed:

```text
APP_ENV=prod sbt run          # PORT is 80 from the prod file
APP_ENV=prod PORT=8443 sbt run # PORT is 8443 from the environment
```

The environment variable wins because it is the first source. Other keys still come from the file.

## Notes

- The environment provider reads the key names as they are. The key `PORT` matches the variable `PORT`.
- Keep the files free of secrets. See [How to read secrets](./how-to-read-secrets.md).
- For the layering rules, see [Config providers](./read-from-various-sources.md).
