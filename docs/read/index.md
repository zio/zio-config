---
id: read_index
title:  "Read"
---
You need this import everywhere

```scala mdoc:silent
import zio.IO
import zio.config._, ConfigDescriptor._

```

## A Simple example

We must be fetching the configuration from the environment to a case class (product) in scala. Let it be `MyConfig`

```scala mdoc:silent

case class MyConfig(ldap: String, port: Int, dburl: String)

```
To perform any action using zio-config, we need a configuration description.
Let's define a simple one.


```scala mdoc:silent
val myConfig =
  ((string("LDAP") |@| int("PORT")|@| string("DB_URL")))(MyConfig.apply, MyConfig.unapply)

```

## Read from various sources
