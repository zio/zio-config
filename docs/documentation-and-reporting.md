---
id: documentation-and-reporting
title: "Documentation Generation"
sidebar_label: "Documentation Generation"
---

Every config description can produce a document that lists its settings. `generateDocs` turns a `Config[A]` into a `ConfigDocs` value, `ConfigDocs#toTable` turns that into a `Table`, and the table renders as GitHub or Confluence Markdown. The output is a reference for the people who operate the application: it shows each key, its format and its description.

The document is generated from the description, so it stays in step with the code. A setting you remove from the description disappears from the document.

## Generating a Document

`generateDocs` comes from `import zio.config._`. It works on any description, hand-written or derived:

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig

final case class Server(host: String, port: Int, region: Option[String])

val serverConfig: Config[Server] = deriveConfig[Server]

val docs: ConfigDocs = generateDocs(serverConfig)
```

`Table#toGithubFlavouredMarkdown` renders the table as GitHub Markdown:

```scala mdoc
println(docs.toTable.toGithubFlavouredMarkdown)
```

The output has a fixed shape. It starts with a `## Configuration Details` heading. A case class becomes an `all-of` row that links to a nested table, here `Field Descriptions`, because its fields are combined with `Config#zip`. Each leaf key has the format `primitive`, and its description starts with a short text from ZIO, such as `a text property` or `an integer property`. The `Sources` column is always empty in ZIO Config 4.

## Adding Descriptions

`Config#??` attaches text to a key. In a derived description, the `@describe` annotation does the same. The text is added after the built-in description:

```scala mdoc:silent
import zio.config.magnolia.describe

@describe("Settings for the HTTP listener")
final case class Listener(
  @describe("Host name or IP address to bind") host: String,
  @describe("Port number, 1024 or above") port: Int
)
```

In the generated table, each field shows the built-in text, then its own description, then the description of the case class:

```scala mdoc
println(generateDocs(deriveConfig[Listener]).toTable.toGithubFlavouredMarkdown)
```

On Scala 3, `@describe` on the case class itself is currently ignored; field descriptions work on both versions.

A hand-written description uses `Config#??` on each key, and on the whole description:

```scala mdoc:silent
val handWritten: Config[Server] =
  ((Config.string("HOST") ?? "Host name") zip
    (Config.int("PORT") ?? "Port number") zip
    (Config.string("REGION").optional ?? "Region, optional")).to[Server] ?? "The HTTP server"
```

Nested descriptions produce one table per level. A prefix added with `Config#nested` becomes a row named after the prefix, which links to a table with the keys under it:

```scala mdoc
println(generateDocs(handWritten.nested("server")).toTable.toGithubFlavouredMarkdown)
```

## Formats

The `Format` column tells the reader what shape of value a key expects. These are the values of `Table.Format`:

| `Format`         | Rendered as         | Produced by                                                       |
| ---------------- | ------------------- | ----------------------------------------------------------------- |
| `Primitive`      | `primitive`         | A single value such as `Config.string` or `Config.int`            |
| `Nested`         | `nested`            | A prefix that directly contains another prefix, such as `nested("app", "db")` |
| `AllOf`          | `all-of`            | `Config#zip`: the keys in the nested table are read together, including optional ones |
| `AnyOneOf`       | `any-one-of`        | `Config#orElse`, a sealed trait, or a key with a default: one of the alternatives is enough |
| `List`           | `list`              | `Config.listOf` and the other sequence descriptions               |
| `Map`            | `map`               | `Config.table`, or `Config#switch`                                |
| `Recursion`      | `recursion`         | Defined for recursive structures; `generateDocs` does not produce it in ZIO Config 4 |
| `RecursionList`  | `list of recursion` | Defined for recursive lists; `generateDocs` does not produce it in ZIO Config 4      |
| `NotApplicable`  | empty               | A row with no format                                              |

A sealed trait or `Config#orElse` produces an `any-one-of` row. For example, a choice between two database settings:

```scala mdoc:silent
val database: Config[String] =
  (Config.string("url") ?? "Full JDBC URL") orElse (Config.string("host") ?? "Host name only")
```

The `db` row has the format `any-one-of`, and its table lists both alternatives:

```scala mdoc
println(generateDocs(database.nested("db")).toTable.toGithubFlavouredMarkdown)
```

## Rendering Markdown

`Table` has three rendering functions:

| Function                                  | Output                                                                                                       |
| ----------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| `Table#toGithubFlavouredMarkdown`         | Markdown with GitHub-style heading anchors; it also renders on GitLab                                        |
| `Table#toConfluenceMarkdown(baseUrl)`     | Confluence-flavoured Markdown; `baseUrl: Option[String]` is the URL of the Confluence page, used to build the links |
| `Table#toMarkdown(getLink)`               | Markdown with links built by your own function                                                               |

To render for a Confluence page:

```scala mdoc
println(docs.toTable.toConfluenceMarkdown(Some("https://wiki.example.com/display/OPS/Config")))
```

`Table#toMarkdown` takes a function `(Table.Heading, Int, Either[Table.FieldName, Table.Format]) => Table.Link`. `Table.githubFlavoured` and `Table.confluenceFlavoured(baseUrl)` are the two built-in functions, and `Table.Link.githubLink`, `Table.Link.confluenceLink` and `Table.Link.rawString` build links. A function that never links gives plain text in the first two columns:

```scala mdoc
println(docs.toTable.toMarkdown((_, _, nameOrFormat) => Table.Link.rawString(nameOrFormat.fold(_.asString(None), _.asString))))
```

## Working With the Table

`Table` is a case class with a list of `TableRow` values, so you can read it or change it before rendering. Each `TableRow` has these fields:

| Field         | Type                             | Meaning                                                    |
| ------------- | -------------------------------- | ---------------------------------------------------------- |
| `paths`       | `List[Table.FieldName]`          | The key path of the row; `FieldName.Blank` for a root row  |
| `format`      | `Option[Table.Format]`           | The format, if known                                       |
| `description` | `List[ConfigDocs.Description]`   | Built-in and user descriptions                             |
| `nested`      | `Option[Table]`                  | The nested table that a link points to                     |
| `sources`     | `Set[String]`                    | Always empty in ZIO Config 4                               |

To list every leaf key with its descriptions, walk the rows and their nested tables:

```scala mdoc
def leafKeys(table: Table): List[String] =
  table.rows.flatMap { row =>
    row.nested match {
      case Some(nested) => leafKeys(nested)
      case None         => List(row.paths.map(_.asString(None)).mkString(".") + ": " + row.description.map(_.description).mkString(", "))
    }
  }

leafKeys(generateDocs(handWritten).toTable).foreach(println)
```

`Table#++` joins two tables. `Table#withFormat` and `Table#mapFormat` change the format column of every row, for example to hide it with `Table.Format.NotApplicable`.

`ConfigDocs` is the tree that `generateDocs` builds before it becomes a table. Its cases follow the shape of the description: `Leaf` for a primitive, `Nested` for a prefix, `Zip` for `Config#zip`, `OrElse` for `Config#orElse`, `Sequence` for a list, and `DynamicMap` for a table or a switch. The case classes are private to `zio.config`, so call `ConfigDocs#toTable` and work with the `Table` instead.

## Writing the Document to a File

Write the rendered string to a file with any file API. A common setup is a small program, run in CI, that writes `docs/config.md` from the description and fails the build when the file changes:

```scala mdoc:compile-only
import java.nio.file.{Files, Paths}

val write: Task[Unit] =
  ZIO.attempt {
    Files.writeString(Paths.get("config.md"), generateDocs(handWritten).toTable.toGithubFlavouredMarkdown)
    ()
  }
```

## Limits

`generateDocs` does not support recursive descriptions built with `Config.defer`: it keeps following the recursion and never returns. Generate documents only for descriptions without recursion.

## Next Steps

- [Derivation](./automatic-derivation-of-config.md) explains the `@describe` annotation.
- [Config descriptors](./config-descriptors.md) explains `Config#??` and the other combinators.
