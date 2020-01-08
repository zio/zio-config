---
id: configdescriptorusage_index
title:  "Using ConfigDescriptor"
---

## Using Config Descriptor

Given a single [ConfigDescriptor](../configdescriptor/index.md) we can use it to:

1. Get Readers that can read config from various sources
2. Get Writer that can write config back
3. Document the config
4. Create report on the config


## Reader from Config Descriptor

You should be familiar with reading config from various sources, given a  config descriptor.

```scala mdoc:silent
import zio.IO
import zio.config._, ConfigDescriptor._

```

```scala mdoc:silent

case class MyConfig(ldap: String, port: Int, dburl: String)

```

To perform any action using zio-config, we need a configuration description.
Let's define a simple one.


```scala mdoc:silent

val myConfig =
  ((string("LDAP") |@| int("PORT")|@| string("DB_URL")))(MyConfig.apply, MyConfig.unapply)

read(myConfig from ConfigSource.fromMap(Map()))  

```

## Writer from config descriptor