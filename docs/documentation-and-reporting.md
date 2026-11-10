---
id: documentation-and-reporting
title: "Documentation and Reporting"
sidebar_label: "Documentation and Reporting"
---

Every config description can produce a document that lists its settings. Give the description to `generateDocs`, then render the result as a table or as Markdown. The output is a reference for operators: it shows each key, its format and, when you add one, its description.

The document is generated from the description, so it stays in step with the code. A setting you remove from the description disappears from the document.

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig

final case class Server(host: String, port: Int, region: Option[String])

val serverConfig: Config[Server] = deriveConfig[Server]
```

## Generating the document

`generateDocs` returns a `ConfigDocs` value. Render it in one of the formats below:

```scala mdoc:silent
val docs: ConfigDocs = generateDocs(serverConfig)

val markdown: String = docs.toTable.toGithubFlavouredMarkdown
```

`toTable` returns a table you can format further. `toGithubFlavouredMarkdown` renders it as a GitHub Markdown table, and `toMarkdown` and `toConfluenceMarkdown` render the other common formats.

Printing `markdown` gives a table with one row per key, similar to this:

```text
| FieldName | Format | Description | Sources |
|:----------|:-------|:------------|:--------|
| host      | string |             |         |
| port      | int    |             |         |
| region    | string |             |         |
```

The `Sources` column is empty here. It lists the sources a key is read from when the description has source information.

## Adding descriptions

A description that comes from a hand-written `Config` can carry text with `??`. Derived configs take the text from `@describe`:

```scala mdoc:silent
import zio.config.magnolia.describe

@describe("Settings for the HTTP server")
final case class Listener(
  @describe("Host name or IP address to bind") host: String,
  @describe("Port number, 1024 or above") port: Int
)

val listenerDocs: String =
  generateDocs(deriveConfig[Listener]).toTable.toGithubFlavouredMarkdown
```

Hand-written descriptions use `??`:

```scala mdoc:silent
val handWritten: Config[Server] =
  (Config.string("HOST") ?? "Host name" zip Config.int("PORT") ?? "Port number" zip
    Config.string("REGION").optional ?? "Region, optional").to[Server]
```

The same `generateDocs` call works on both.

## Writing the document to a file

Write the rendered string to a file with any file API. A common setup is a small program, run in CI, that writes `docs/config.md` from the description and commits it when it changes.

```scala mdoc:compile-only
import java.nio.file.{Files, Paths}

val write: Task[Unit] =
  ZIO.attempt {
    Files.writeString(Paths.get("config.md"), generateDocs(handWritten).toTable.toGithubFlavouredMarkdown)
    ()
  }
```

## Next steps

- [Derivation](./automatic-derivation-of-config.md) explains the `@describe` annotation.
- [Config descriptors](./config-descriptors.md) explains `??` and the other combinators.
