# ZIO Config Documentation Gap Report

Audit of `docs/` against the source on `series/4.x` at 4f1ef5c (zio-config 4.1.2, ZIO 2.1.26).
This file is an internal tracking document. It is not published (it lives outside `docs/`).

## How this audit was done

1. Ran the `docs-find-documentation-gaps` scanner over the whole repository.
2. Read every page in `docs/` (17 pages, 1,753 lines) and every main source file in every published module.
3. Checked suspicious statements by running code against the published 4.1.2 artifacts with scala-cli, on Scala 2.13.16 and Scala 3.9.0. Each finding marked **(verified)** below was reproduced that way.

## Summary

| Metric | Count |
| --- | --- |
| Pages in `docs/` | 17 (1 tutorial, 4 how-to guides, 10 reference, getting started, introduction) |
| Public types found by the scanner | 73 |
| Public types named anywhere in the docs (scanner) | 7 (9%) |
| Packages with no page that names their main type | 6 (`aws.parameterstore`, `derivation`, `refined`, `toml`, `typesafe`, `yaml`) |
| Wrong or outdated statements in existing pages | 19 numbered (F1–F19) on 8 pages, plus 4 smaller issues on 4 more pages |
| Code examples that do not compile | 1 found (cats `mapN`); 13 Scala code blocks on 4 pages are not compiled by mdoc at all (derivation 5, integrations 5, pureconfig guide 2, AWS 1; sbt dependency snippets not counted) |
| Reference topics missing or thin | 9 |
| Tutorials missing | 2 proposed |
| How-to guides missing | 6 proposed |
| Suspected library bugs found (not fixed here) | 7 |

The scanner's 9% figure is too pessimistic: it counts sum-type cases such as `ConfigDocs.Leaf` and `Table.Format.List` as separate types. Most of the
important user-facing surface (`Config`, `ConfigProvider`, `deriveConfig`, `generateDocs`, the provider constructors) is mentioned somewhere.
The real problems are different. Several pages make statements that the code contradicts. Whole features have no docs at all, such as YAML
environment-variable substitution, `@prefix`/`@postfix`, case-class default values, `ZIO.config` with `Runtime.setConfigProvider`, and the `read(... from ...)`
API. There is also no per-type reference for `DeriveConfig`, `ConfigDocs`/`Table` or the providers of each source module.

---

## 1. Fixes to existing pages (wrong or outdated statements)

Each item has the page and line, what the page says, what the code does, and the evidence.

### config-descriptors.md

- [ ] **F1. `Config.constant` is described wrongly** (`docs/config-descriptors.md:109`). The page says it "always succeeds with the given value and reads nothing".
  The zio-config extension `Config.constant(value)` (`core/shared/src/main/scala/zio/config/syntax/ConfigSyntax.scala:451`) reads a string at the current path and
  succeeds only when that string equals `value`. Loading `Config.constant("hello")` from an empty map fails with `MissingValue` **(verified)**. The
  "always succeeds, reads nothing" value is ZIO's `Config.succeed`. *Scope: update existing.*
- [ ] **F2. `zio-config` extensions are not marked as such.** `Config.byte`, `Config.short`, `Config.uuid`, `Config.constant`, `Config.collectAll`,
  `orElseEither`, `mapKey`, `toKebabCase`/`toSnakeCase`/`toUpperCase`/`toLowerCase` and `to[B]` come from `import zio.config._`. They are not in ZIO core.
  Readers who copy an example without the import get "value X is not a member of object zio.Config". *Scope: update existing (one note + import).*

### errors.md

- [ ] **F3. The error-kind table is incomplete** (`docs/errors.md:13`). `zio.Config.Error` also has `And` and `Or`, the cases that make accumulation work.
  The sentence "Each one carries the key path" is false for `And`/`Or`. *Scope: update existing.*
- [ ] **F4. The `prettyPrint` sample is wrong** (`docs/errors.md:52`). For a single failure, `prettyPrint` prints no tree
  (`ConfigSyntax.scala:155-160`). The real output is shown below **(verified)**. The page shows a tree with `╥`/`╠─` and `Details: value of type int`. That
  text does not exist in the source. *Scope: update existing; generate the sample with mdoc so it can't drift.*
  ```text
  ReadError:
  MissingValue
  path: port
  Expected port to be set in properties
  ```
- [ ] **F5. `SourceUnavailable` example is misleading** (`docs/errors.md:17`). A missing HOCON/YAML/TOML file makes `fromHoconFile` and the others *throw*. The
  `...ZIO` variants fail with a `Throwable`, not with a `Config.Error`. AWS failures are mapped to `Unsupported`, not to `SourceUnavailable`
  (`aws/.../ParameterStoreConfigProvider.scala:58`). *Scope: update existing.*

### documentation-and-reporting.md

- [ ] **F6. The sample output table is wrong** (`docs/documentation-and-reporting.md:35`). Real output for the page's own `Server` example **(verified)**:
  the document starts with a `## Configuration Details` heading. The top row has format `all-of` and links to a nested `### Field Descriptions` table.
  The format of a leaf is `primitive`, not `string`/`int`. The description column holds built-in text such as `a text property` and `an integer property`.
- [ ] **F7. Rendering methods are described loosely** (`docs/documentation-and-reporting.md:31`). `toConfluenceMarkdown` takes a required
  `baseUrl: Option[String]`. `toMarkdown` takes a link-building function, not "another common format". *Scope: update existing.*
- [ ] **F8. "Sources" column** (`docs/documentation-and-reporting.md:43`). In 4.x `generateDocs` never fills the sources (`ConfigDocsModule.scala:103,117,144` use
  `Set.empty`), so the column is always empty. *Scope: update existing.*

### automatic-derivation-of-config.md

- [ ] **F9. `@kebabCaseLegacy` examples show no difference** (`docs/automatic-derivation-of-config.md:152`). The page says legacy rules give
  `myValue123 → my-value123` and `QAndA → qand-a`, which implies `@kebabCase` gives something else. In 4.1.2, `@kebabCase` gives the **same** keys for both
  examples. The two annotations differ only where a digit or other non-letter is followed by an uppercase letter: `version2Name` becomes `version2-name`
  with `@kebabCase` and `version2name` with `@kebabCaseLegacy` **(verified on Scala 2.13 and 3.9)**. *Scope: update existing.*
- [ ] **F10. Value classes** (`docs/automatic-derivation-of-config.md:202`). "For ... a value class, write an instance yourself" is true on Scala 3 only. On Scala 2,
  `final case class W(value: String) extends AnyVal` is derived automatically and unwrapped (the field reads its parent key directly). On Scala 3 there is no
  instance, and compilation fails **(verified)**. *Scope: update existing.*
- [ ] **F11. Annotation import column** (`docs/automatic-derivation-of-config.md:93`). On Scala 3, `zio.config.magnolia` re-exports only `describe`, `name`,
  `discriminator` and `kebabCaseLegacy`. `kebabCase`, `snakeCase`, `prefix` and `postfix` must come from `zio.config.derivation`
  (`magnolia/shared/src/main/scala-dotty/zio/config/magnolia/package.scala`). Recommend `zio.config.derivation._` for every annotation, because it works on both
  Scala versions. *Scope: update existing.*
- [ ] **F12. Two sealed-trait code blocks and the Scala 3 block are plain ```` ```scala ```` and are not compiled** (lines 56, 72, 166, 184, 225). *Scope: update existing
  (switch to mdoc).*

### whats-new-since-4-0-6.md

- [ ] **F13. Release order** (`docs/whats-new-since-4-0-6.md`). Sections run 4.1.0, 4.0.8, 4.0.7, then 4.1.1/4.1.2. Use newest-first order.
- [ ] **F14. Same kebab-case example as F9** (`docs/whats-new-since-4-0-6.md:19`). The link text "Read from various Sources" does not match the page title "Config Providers".

### read-from-various-sources.md

- [ ] **F15. Comma splitting default** (`docs/read-from-various-sources.md:22`). "Every provider can also take a flag `enableCommaSeparatedValueAsList` ...
  It defaults to `false`." That is true for the zio-config modules (HOCON, YAML, TOML) only. ZIO core's `ConfigProvider.fromMap` and `fromEnv` split on `,` by
  default (`seqDelim = ","`): `fromMap(Map("xs" -> "1,2,3"))` loads `listOf("xs", int)` as `List(1, 2, 3)`, while the HOCON provider fails on the same value
  **(verified)**. *Scope: update existing.*
- [ ] **F16. "Each `from...` method has a `...ZIO` variant"** is not true for `fromYamlRepr` or `XmlConfigProvider.fromXmlString`. `fromResourcePath` uses
  `ConfigFactory.load`, so it also merges `reference.conf` and JVM system properties. The page does not say this. *Scope: update existing.*

### integrations.md

- [ ] **F17. The cats `mapN` example does not compile** (`docs/integrations.md:50`): `could not find implicit value for parameter semigroupal: cats.Semigroupal[zio.Config]`
  **(verified)**. The instance is an `Applicative`, but it is declared as `Functor[Config]` (`cats/shared/src/main/scala/zio/config/cats/instances/package.scala:8`),
  so only `Functor` syntax is visible. See bug B1. Until that is fixed, the page must show what works. *Scope: update existing.*
- [ ] **F18. None of the examples on this page are compiled.** The docs project has no dependency on cats, scalaz, enumeratum or pureconfig (`build.sbt:546-590`).
  The page says so in its last line. *Scope: needs a `build.sbt` change to the `docs` project (outside `docs/`, needs approval).*

### how-to-migrate-from-pureconfig.md

- [ ] **F19. `fromPureconfig` does not read a structured object.** It reads **one string value** at the current path, parses that string as HOCON, and hands it to the
  `ConfigReader` (`pureconfig/shared/src/main/scala/zio/config/pureconfig/package.scala:10`). The guide's `Database` example uses `forProduct2` and expects to load
  `url`/`poolSize` keys through a normal provider. That fails, because the provider has no single string value there. Also, pureconfig's default key style is
  **kebab-case**: its default `ProductHint` maps `poolSize` to `pool-size`. So "Keys follow the field names, as with pureconfig's default" is wrong, and a direct
  migration needs `@kebabCase` or `.toKebabCase`. The mapping table also misses the case-object difference: pureconfig expects `x { type = a }`, while zio-config
  reads `x = A` (`examples/.../typesafe/PureConfigInterop.scala:19`). *Scope: rewrite.*

### Smaller issues

- [ ] `tutorial-hocon-and-env.md:23-49`: Step 1 defines `ServiceConfig`, then Step 2 defines an identical `Service`, and `ServiceConfig` is never used again.
- [ ] `how-to-read-secrets.md:64`: `c.password.value.mkString` works, but ZIO 2.1 has `Secret#stringValue`. Mention the simpler form.
- [ ] `how-to-override-per-environment.md:90`: "The environment provider reads the key names as they are" holds only for flat keys. `envProvider` uses `_` as the path
  delimiter, so a nested key `db.url` is read from `DB_URL`, and it needs `.toUpperCase` or `mapKey` on the description.
- [ ] `resources.md`: a 14-line stub. Keep it, but fix the trailing "PS:" note.

---

## 2. Reference pages to write or expand

Priority: **Critical** = users hit it on day one; **High** = common; **Medium** = advanced.

- [ ] **R1 (Critical). Core `Config` API and zio-config extensions.** *Expand `config-descriptors.md`, or split it into "Config descriptors" + "zio-config syntax".*
  Missing: ZIO core `validate`, `validateWith`, `switch`, `Config.succeed`, `Config.fail`, `secret`, `duration`, `setOf`, `vectorOf`, `chunkOf`,
  `nonEmptyChunkOf`, `nested(name, names*)`, `??`. Also the zio-config additions `Config.byte`/`short`/`uuid`, `collectAll`, `constant` (correct semantics),
  `orElseEither`, `mapKey`, `to[B]`, `read(config from provider)` with `Read`/`ReadZIO` (the 3.x-compatible API), `toConfigProviderZIO`, the `path"a.b.c"`
  interpolator, and the key functions `toKebabCase`, `toKebabCaseLegacy`, `toSnakeCase`, `camelToDelimiter`, `addPrefixToKey`, `addPostFixToKey`.
  Sources: `core/shared/src/main/scala/zio/config/{package.scala,syntax/ConfigSyntax.scala,KeyConversionFunctions.scala}`.
- [ ] **R2 (Critical). `ConfigProvider` from ZIO core and how zio-config plugs into it.** *New page.* `ZIO.config(desc)` with `Runtime.setConfigProvider(...)` as a
  bootstrap layer (the idiomatic ZIO 2 pattern, not mentioned anywhere in the docs), `fromEnv(pathDelim, seqDelim)`, `fromProps`, `fromAppArgs`, `fromMap(pathDelim, seqDelim)`,
  and the transforms `nested`, `unnested`, `kebabCase`, `snakeCase`, `upperCase`, `lowerCase`, `contramapPath`, `within`, `orElse`.
- [ ] **R3 (Critical). `DeriveConfig` reference.** *New page.* Built-in instances: `String`, `Boolean`, `Int`, `Long`, `Short`, `Byte`, `BigInt`, `Float`, `Double`, `BigDecimal`,
  `URI`, `UUID`, `Duration`, `LocalDate`, `LocalTime`, `LocalDateTime`, `OffsetDateTime`, `LogLevel`, `Secret`, `Option`, `Either`, `List`, `Seq`, `Set`, `Vector`,
  `Chunk`, `NonEmptyChunk`, `Map[String, A]`. Methods: `??`, `describe`, `map`, `mapAttempt`, `mapOrFail`, `DeriveConfig[A]`, and Scala 3 `DeriveConfig.from`.
  Behaviours: case-class default values become `withDefault` **(verified)**; `@prefix`/`@postfix` work only on the class, not on a field **(verified)**;
  `Either` via `orElseEither`; duplicate subtype names become `Name_0`, `Name_1`; Scala 3 only: `autoLoad[A]`, `derives Config`, and an implicit `Config[A]`
  used as a `DeriveConfig[A]`. Add a Scala 2 vs Scala 3 differences table: value classes, `@describe` on a class (bug B2), re-exported annotations.
  Sources: `magnolia/shared/src/main/scala-2.12-2.13/.../DeriveConfig.scala`, `magnolia/shared/src/main/scala-dotty/.../{DeriveConfig,package}.scala`, `derivation/.../annotations.scala`.
- [ ] **R4 (High). Documentation generation: `ConfigDocs` and `Table`.** *Expand `documentation-and-reporting.md` into a reference.* `generateDocs`; the `ConfigDocs`
  cases (`Leaf`, `Nested`, `Zip`, `OrElse`, `Sequence`, `DynamicMap`); `Table`, `TableRow`, `Format` (`all-of`, `any-one-of`, `list`, `map`, `primitive`, …),
  `FieldName`, `Heading`, `Link`; `toGithubFlavouredMarkdown`, `toConfluenceMarkdown(baseUrl)`, `toMarkdown(getLink)`, `mapFormat`, `withFormat`, `++`.
  Known limit: `generateDocs` loops forever on a recursive (`Config.defer`) description (open PR #1742). Source: `core/shared/src/main/scala/zio/config/ConfigDocsModule.scala`.
- [ ] **R5 (High). One reference page per source module**, with `read-from-various-sources.md` kept as the overview/chooser:
  - HOCON/JSON (`typesafe`): `fromTypesafeConfig`/`fromTypesafeConfigZIO`, `TypesafeConfigProvider`, `ConfigFactory.load` semantics, `null` treated as missing.
  - YAML: **environment-variable substitution** `${VAR}`, `${VAR:-default}`, `${VAR:?message}` (`yaml/.../EnvConfigImpl.scala`, **verified**), which is not documented at all;
    `fromYamlRepr`, `fromYamlReader`, an empty list stored as `<nil>`, `YamlConfigProvider`.
  - TOML: `fromToml(TomlParseResult)`/`fromTomlZIO`, `fromTomlReader`, dates and times read as strings, parse errors thrown.
  - XML (experimental): keep it short; `XmlParser`/`XmlObject` are internal.
- [ ] **R6 (High). Errors as a reference.** Follows on from F3–F5: all six cases, how `zip` builds `And` and `orElse` builds `Or`, the real `prettyPrint` output for
  single, `And` and `Or` errors, and `prettyPrint(keyDelimiter)`.
- [ ] **R7 (High). Integrations, one section (or page) per module, with compiled examples.**
  - cats: `chain`, `nonEmptyChain`, `nonEmptyList` and `nonEmptyMap` (each with and without a path), `SemigroupK[Config]` (`<+>` = `orElse`), `Semigroup[ConfigProvider]` (`|+|` = `orElse`), `Eq[Config.Error]`.
  - scalaz: `Applicative[Config]`; `iList`, `iSet` (needs `Order`), `mapz` (takes a `Config[Map[A, B]]`), `maybe`.
  - enumeratum: `enum`, `intEnum`, `longEnum`, `shortEnum`, `stringEnum`, `byteEnum`, `charEnum`, `nonEmptyString`.
  - pureconfig: what `fromPureconfig` really does (see F19).
- [ ] **R8 (Medium). Refined.** Expand `automatic-validations.md`. It documents two forms, but there are three: `refine[A, P](path)` (reads a path with a derived `A`) is missing.
  Also missing: the implicit `deriveRefinedDescriptor`, and how `refineType` uses `DeriveConfig` (`refined/shared/src/main/scala/zio/config/refined/`).
- [ ] **R9 (Medium). AWS Parameter Store.** The page is good. Add the paging behaviour, that the base path is used as a regular expression (bug B7), and the error case (`Unsupported`).

## 3. Tutorials (learning-oriented)

There is one tutorial today. Proposed additions:

- [ ] **T1. Model a real config with case classes, sealed traits, lists and maps.** It walks from a flat case class to nested products, an optional field, a default value,
  a `List`, a `Map`, and a sealed trait with and without `@discriminator`, loading each step from HOCON. *New page.*
- [ ] **T2. Load configuration in a ZIO application with layers.** It uses `ZIO.config`, `Runtime.setConfigProvider` in `bootstrap`, a `ZLayer` built from config,
  and swapping the provider in tests. *New page.*
- [ ] Fix the existing tutorial (duplicate case class; see the Smaller issues list in section 1).

## 4. How-to guides (task-oriented)

The four existing guides stay. Proposed additions:

- [ ] **H1. Use zio-config with ZIO's built-in `ZIO.config`.** Set the provider once, then read config anywhere.
- [ ] **H2. Read lists and maps from environment variables or other flat sources.** Covers indexed keys `servers[0]`, `seqDelim`, comma-separated values, and nested keys with `_`.
- [ ] **H3. Read a custom type.** Covers `mapOrFail`, `validate`, a hand-written `DeriveConfig` instance, value classes on Scala 2 vs 3, and a refined type.
- [ ] **H4. Generate configuration docs in CI.** Writes the Markdown table to a file and fails the build when it changes.
- [ ] **H5. Use environment variables inside a YAML file.** Covers `${VAR}`, `${VAR:-default}` and `${VAR:?message}`.
- [ ] **H6. Migrate from zio-config 3.x to 4.x.** Covers `ConfigDescriptor` → `Config`, `read(desc from source)` (still supported), `ConfigSource` → `ConfigProvider`, and
  `descriptor[A]` → `deriveConfig[A]`. The README for 3.x users points nowhere today.

## 5. Low priority or skip

- `IndexedFlat`, `KeyComponent`, `ConfigPath` (`core/.../IndexedFlat.scala`): public, but used only inside the providers, and the indexed `ConfigProvider` that would expose them
  is commented out (`ConfigSyntax.scala:226`). At most a one-line mention.
- `ConfigSyntax.ChunkOps.mapLast`, `TupleConversion`, `ImplicitTupleConversion`, `PropertyTypePlatformSpecific`, `VersionSpecificSupport`, `NeedsDerive`, `Macros`:
  implementation details. Skip.
- `xml.experimental.{Parsers, XmlParser, XmlObject, TagElement, Text}`: internals of an experimental module. Skip.
- `DeriveConfig.Metadata`, `ProductName`, `CoproductName`, `FieldName`, `KeyModifier`, `CaseModifier`, `wrapSealedTrait`, `prepare*`, `combine`, `dispatch`: derivation
  internals. They are public, but users do not call them. Skip.

## 6. Suspected library bugs (reported to the coordinator, not fixed here)

- **B1. cats `Applicative` instance hidden behind `Functor`.** `cats/shared/src/main/scala/zio/config/cats/instances/package.scala:8` declares
  `implicit val cfgDescInvariantSemiGroupal: Functor[Config] = new Applicative[Config] {...}`, so `mapN`, `tupled` and `ap` do not resolve. Repro: the
  `integrations.md` cats example fails to compile with `could not find implicit value for parameter semigroupal: cats.Semigroupal[zio.Config]` (4.1.2, Scala 2.13.16).
- **B2. Scala 3 derivation drops `@describe` on a case class or sealed trait.** `ProductName.descriptions` and `CoproductName.descriptions` are computed but never applied
  (`magnolia/shared/src/main/scala-dotty/zio/config/magnolia/DeriveConfig.scala:191,314-328`). Repro:
  `@describe("whole") case class Desc(@describe("the port") port: Int)` generates the description `an integer property, the port, whole` on Scala 2.13 and
  `an integer property, the port` on Scala 3.9.
- **B3. Wrong scaladoc examples for the kebab-case functions.** `core/shared/src/main/scala/zio/config/KeyConversionFunctions.scala:40-53` says `toKebabCase("myValue123") == "my-value-123"`
  and `"QAndA" -> "q-and-a"`. The real results are `my-value123` and `qand-a`, the same as `toKebabCaseLegacy`. Related: PR #1650 (`@kebabCaseLegacy`) said
  `@kebabCase` turns `instantIdQAndA` into `instant-id-q-and-a`. In 4.1.2, both annotations give `instant-id-qand-a`. The only difference is at digit→uppercase boundaries.
  Someone should check whether issue #1612 is really fixed.
- **B4. `xml.experimental` adds `ConfigProvider.fromYamlString`, which reads XML.** It is a misnamed method (`xml/.../experimental/package.scala:7`). The docs already warn about it.
- **B5. `generateDocs` loops forever on recursive descriptions.** It does not check `alreadySeen` for `Config.Lazy` (`ConfigDocsModule.scala:580`). Open contributor PR #1742 fixes it.
- **B6. `@prefix`/`@postfix` on a field are silently ignored.** Only class-level annotations are read (Scala 2 `DeriveConfig.scala:145-157`, Scala 3 `keyModifiersOfImpl`). Possibly by
  design, but nothing warns the user. Repro: `case class P(@prefix("my") port: Int)` reads `port`, not `myPort`.
- **B7. AWS base path is used as a regular expression.** `convertParameterListToMap` uses `replaceFirst(s"$basePath/", "")`
  (`aws/.../ParameterStoreConfigProvider.scala:62`, `zio-aws/.../ParameterStoreConfigProvider.scala:35`). A base path with regex metacharacters strips the wrong prefix.

## 7. Proposed sidebar

```text
Introduction
Getting Started
Tutorials
  Configure an app from HOCON and environment variables   (existing, fixed)
  Model a real config with ADTs                            (T1, new)
  Load configuration in a ZIO application                  (T2, new)
How-to Guides
  Read secrets                                             (existing)
  Override settings per environment                        (existing, fixed)
  Test config                                              (existing)
  Use zio-config with ZIO.config                           (H1, new)
  Read lists and maps from flat sources                    (H2, new)
  Read a custom type                                       (H3, new)
  Generate configuration docs in CI                        (H4, new)
  Use environment variables in YAML                        (H5, new)
  Migrate from pureconfig                                  (existing, rewritten)
  Migrate from zio-config 3.x                              (H6, new)
Reference
  Config descriptors                                       (R1, expanded)
  Config providers (overview)                              (existing, fixed)
    ZIO core providers                                     (R2, new)
    HOCON and JSON                                         (R5, new)
    YAML                                                   (R5, new)
    TOML                                                   (R5, new)
    XML (experimental)                                     (R5, new)
    AWS Parameter Store                                    (existing, R9)
  Derivation                                               (existing, fixed)
  DeriveConfig                                             (R3, new)
  Validation (refined)                                     (R8, expanded)
  Documentation generation                                 (R4, expanded)
  Errors                                                   (R6, expanded)
  Integrations (cats, scalaz, enumeratum, pureconfig)      (R7, expanded)
  What's new                                               (existing, fixed)
  Resources                                                (existing)
```

## 8. Suggested order of work (each step is one PR)

1. **Fixes** (section 1): F1–F19 and the smaller issues. They are small and easy to review, and they stop wrong statements reaching readers now.
2. **Core reference**: R1, R2, R6.
3. **Derivation reference**: R3, the derivation-page fixes, R8.
4. **Provider and documentation-generation reference**: R4, R5, R9.
5. **Integrations**: R7 plus the `build.sbt` docs-dependency change (F18), which needs approval.
6. **Tutorials**: T1, T2.
7. **How-to guides**: H1–H6.
