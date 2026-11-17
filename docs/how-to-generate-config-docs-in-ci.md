---
id: how-to-generate-config-docs-in-ci
title: "How to Keep a Configuration Reference in Sync With CI"
sidebar_label: "Generate Config Docs in CI"
---

Operators need a list of every setting an application reads. ZIO Config can generate that list from the config description, so it can't fall behind the code. This guide writes the list to a Markdown file and makes CI fail when someone changes the description without updating the file.

## Goal

Keep `docs/configuration.md` in the repository, generated from the description, and checked on every build.

## Step 1: Describe Every Setting

Add a description to each key with `@describe` on derived case classes, or `Config#??` on hand-written descriptions. The text goes into the generated table:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.derivation.describe
import zio.config.magnolia.deriveConfig

final case class Database(
  @describe("JDBC URL of the primary database") url: String,
  @describe("Maximum number of open connections") poolSize: Int = 10
)

final case class AppConfig(
  @describe("Port the HTTP server listens on") port: Int,
  database: Database
)

val appConfig: Config[AppConfig] = deriveConfig[AppConfig]
```

## Step 2: Render the Table

`generateDocs` builds the document and `Table#toGithubFlavouredMarkdown` renders it:

```scala mdoc
println(generateDocs(appConfig).toTable.toGithubFlavouredMarkdown)
```

A field with a default value, such as `poolSize`, appears as an `any-one-of` row: the key itself, or the default, which the table lists as `a constant property`. Mention the default in the field's `@describe` text if operators should see its value.

For a Confluence page, use `Table#toConfluenceMarkdown` instead; see [Documentation generation](./documentation-and-reporting.md#rendering-markdown).

## Step 3: Write It From a Small Program

Add a program to the project that writes the file. With a `--check` argument, it compares the file with the freshly generated text and fails if they differ:

```scala mdoc:compile-only
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

object GenerateConfigDocs extends ZIOAppDefault {
  val target: Path = Paths.get("docs/configuration.md")

  def run =
    for {
      args     <- getArgs
      expected  = generateDocs(appConfig).toTable.toGithubFlavouredMarkdown
      _        <- if (args.contains("--check")) check(expected) else write(expected)
    } yield ()

  def write(text: String): Task[Unit] =
    ZIO.attempt(Files.write(target, text.getBytes(StandardCharsets.UTF_8))).unit

  def check(expected: String): Task[Unit] =
    ZIO.attempt(new String(Files.readAllBytes(target), StandardCharsets.UTF_8)).flatMap { actual =>
      ZIO.fail(new RuntimeException(s"$target is out of date; run GenerateConfigDocs")).unless(actual == expected).unit
    }
}
```

Run `sbt "runMain GenerateConfigDocs"` to write the file and commit it.

## Step 4: Check It in CI

Add a CI step that runs the check after compiling:

```bash
sbt "runMain GenerateConfigDocs --check"
```

The step fails, and names the file, whenever the description changed but the file did not.

## Notes

- Keep the description in one place, such as a `val` in the companion object of the config class, so that the application and the generator use the same value.
- `generateDocs` can't document a recursive description; see [Documentation generation](./documentation-and-reporting.md#limits).
- [Generated output](./documentation-and-reporting.md#generating-a-document) explains each column of the table.
