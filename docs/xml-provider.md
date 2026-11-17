---
id: xml-provider
title: "XML (Experimental)"
sidebar_label: "XML (Experimental)"
---

The `zio-config-xml` module reads a small subset of XML into a `ConfigProvider`. It is experimental: it has its own small parser, it does not support XML comments, and it can't read lists. Prefer HOCON, YAML or TOML for new projects.

To add the module:

```scala
libraryDependencies += "dev.zio" %% "zio-config-xml" % "@VERSION@"
```

## Building a Provider

`XmlConfigProvider.fromXmlString` in the package `zio.config.xml.experimental` parses a string. It throws if the XML can't be parsed. There is no file or `ZIO` variant; read the file yourself and wrap the call in `ZIO.attempt` when you need one.

The package also adds `ConfigProvider.fromYamlString` for XML input. The name is a mistake in the library; use `XmlConfigProvider.fromXmlString`, which does the same.

## How XML Becomes Keys

The examples on this page use these imports and a helper, `fromXml`:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.xml.experimental._

def fromXml[A](config: Config[A], xml: String): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(XmlConfigProvider.fromXmlString(xml).load(config).either).getOrThrow()
  }

final case class Aws(region: String, account: String)
final case class Database(port: Int, url: String)
final case class Configuration(aws: Aws, database: Database)
```

The key path starts at the root element. Each attribute is a key under its element, so `<config><aws region="us-east"/></config>` produces the key `config.aws.region`. The text of an element is the value of the element's own path, so `<port>123</port>` inside `<database>` produces `config.database.port`. Both styles can be mixed:

```scala mdoc
fromXml(deriveConfig[Configuration].nested("config"), """
<config>
  <aws region="us-east" account="personal"></aws>
  <database url="jdbc:postgresql://localhost/app">
    <port>123</port>
  </database>
</config>
""")
```

Because the root element is part of every key, the description is nested under the root's name, here with `Config#nested("config")`.

Two rules follow from this mapping. When an attribute and a child element have the same name, they produce the same key and the child element wins. Repeated sibling elements with the same name also produce the same key, so only the last one is kept, and there is no way to express a list.

## Next Steps

- [Config providers](./read-from-various-sources.md) compares every source.
