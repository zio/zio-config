---
id: how-to-read-secrets
title: "How to Read Secrets"
sidebar_label: "Read Secrets"
---

A secret is a value such as a password or an API key. This guide shows how to read one without writing it into a file in the repository, and how to keep it out of logs.

## Goal

Read a database password from an environment variable, or from a file that the deployment platform mounts, and never print it.

## Step 1: Describe the Secret as `Config.Secret`

`Config.secret` reads a value into a `Config.Secret`. Its `toString` hides the value, so printing the config does not leak it:

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig

final case class Credentials(user: String, password: Config.Secret)

val credentials: Config[Credentials] =
  (Config.string("DB_USER") zip Config.secret("DB_PASSWORD")).to[Credentials]
```

## Step 2: Load It from the Environment

```scala mdoc:compile-only
val fromEnvironment: IO[Config.Error, Credentials] =
  ConfigProvider.envProvider.load(credentials)
```

Set `DB_USER` and `DB_PASSWORD` in the environment of the process. The value never appears in the source tree.

## Step 3: Read It from a Mounted File

Some platforms put each secret in a file, for example `/run/secrets/db_password`. Read the file's content into a map and load from it. The file's content is read as the value, so strip the trailing newline:

```scala mdoc:compile-only
import java.nio.file.{Files, Paths}

val fromFile: Task[ConfigProvider] =
  ZIO.attempt {
    val password = Files.readString(Paths.get("/run/secrets/db_password")).trim
    ConfigProvider.fromMap(Map("DB_USER" -> "orders", "DB_PASSWORD" -> password))
  }

val loadedFromFile: Task[Credentials] =
  fromFile.flatMap(_.load(credentials).mapError(e => new RuntimeException(e.toString)))
```

## Step 4: Use the Value at the Point of Need

Call `Config.Secret#stringValue` only where the password is passed to a client. Do not store it in a log message or a string:

```scala mdoc:compile-only
def openConnection(user: String, password: String): Task[Unit] = ZIO.unit

val connect: IO[Config.Error, Task[Unit]] =
  ConfigProvider.envProvider
    .load(credentials)
    .map(c => openConnection(c.user, c.password.stringValue))
```

The value is available through `Config.Secret#stringValue`, which returns a `String`, and `Config.Secret#value`, which returns a `Chunk[Char]`.

## Checks

Check that the file containing the secret is not committed (look in `.gitignore`), and that errors from a missing secret name the key, not the value; see [Errors](./errors.md).

## Related

See [Config providers](./read-from-various-sources.md) for each source, and [Tutorial: Configure an App from HOCON and Environment Variables](./tutorial-hocon-and-env.md) for layering secrets and defaults.
