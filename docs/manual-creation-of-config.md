---
id: manual-creation-of-config
title:  "Manual creation of Config"
---

Refer to core ZIO for more documentations around `Config`

## CollectAll (Sequence)

```scala
def database(i: Int) =
  (string(s"${i}_URL") zip int(s"${i}_PORT")).to[Database]

val list: Config[List[Database]] =
  collectAll(database(0), (1 to 10).map(database): _*)
```

Running this to ZIO will result in non empty list of database

NOTE: `collectAll` is a synonym for `sequence`.

## Handling list

```scala
final case class PgmConfig(a: String, b: List[String])

val configWithList =
  (string("xyz") zip listOf("regions")(string)).to[PgmConfig]

Config.fromEnv(configWithList, valueDelimiter = Some(","))
// or read(configWithList from ConfigSource.fromEnv(valueDelimiter = Some(",")))
```

List is probably better represented in HOCON files.
zio-config-typesafe enables you to depend on HOCON files to manage your configuration.

Given;

```scala
val listHocon =
  """
    accounts = [
      {
         region : us-east
         accountId: jon
      }
      {
         region : us-west
         accountId: chris
      }
    ]
    database {
        port : 100
        url  : postgres
    }
  """
```

```scala
import zio.config.typesafe.TypesafeConfigSource._
import zio.config.magnolia._

// A nested example with type safe config, and usage of magnolia
final case class Accnt(region: String, accountId: String)

final case class Db(port: Int, url: String)

final case class AwsDetails(accounts: List[Accnt], database: Db)

val autoListConfig = descriptor[AwsDetails]

read(autoListConfig from hocon(listHocon))

// yields
//  AwsDetails(
//    List(Accnt("us-east", "jon"), Accnt("us-west", "chris")),
//    Db(100, "postgres")
//  )
```

Note that `autoListConfig` (automatically generated) config, is exactly similar to:

```scala
val accnt =
  (string("region") zip string("accountId")).to[Accnt]

val db = (int("port") zip string("url")).to[Db]

val nonAutomatic =
  (nested("accounts")(listOf(accnt)) zip nested("database")(db)).to[AwsDetails]
```

Please find more details on the behaviour of `List` for various sources in `Sources` section of the documentation.
