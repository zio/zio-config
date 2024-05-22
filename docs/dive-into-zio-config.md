---
id: dive-into-zio-config
title: "Dive Into ZIO Config"
---


## Accumulating all errors

For any misconfiguration, the `ReadError` collects all of them with proper semantics: `AndErrors` and `OrErrors`.
Instead of directly printing misconfigurations, the `ReadError.prettyPrint` shows the path, detail of collected misconfigurations.

1. All misconfigurations of `AndErrors` are put in parallel lines.

```text
╥
╠══╗ 
║  ║ FormatError
║ MissingValue
``` 

2. `OrErrors` are in the same line which indicates a sequential misconfiguration

```text
╥
╠MissingValue
║
╠FormatError
```

Here is a complete example:

```text
   ReadError:
   ╥
   ╠══╦══╗
   ║  ║  ║
   ║  ║  ╠─MissingValue
   ║  ║  ║ path: var2
   ║  ║  ║ Details: value of type string
   ║  ║  ║ 
   ║  ║  ╠─MissingValue path: envvar3
   ║  ║  ║ path: var3
   ║  ║  ║ Details: value of type string
   ║  ║  ║ 
   ║  ║  ▼
   ║  ║
   ║  ╠─FormatError
   ║  ║ cause: Provided value is wrong, expecting the type int
   ║  ║ path: var1
   ║  ▼
   ▼
```

### Example of mapping keys

Now on, the only way to change keys is as follows:

```scala
  // mapKey is just a function in `Config` that pre-existed

  val config = deriveConfig[Config].mapKey(_.toUpperCase)
```

## A few handy methods

### CollectAll

```scala mdoc:compile-only
import zio._
import zio.config._

  final case class Variables(variable1: Int, variable2: Option[Int])

  val listOfConfig: List[Config[Variables]] =
    List("GROUP1", "GROUP2", "GROUP3", "GROUP4")
      .map(group => (Config.int(s"${group}_VARIABLE1") zip Config.int(s"${group}_VARIABLE2").optional).to[Variables])

  val configOfList: Config[List[Variables]] =
    Config.collectAll(listOfConfig.head, listOfConfig.tail: _*)
```

### orElseEither && Constant

```scala mdoc:compile-only
import zio._
import zio.config._ 

sealed trait Greeting

case object Hello extends Greeting
case object Bye extends Greeting

val configSource = 
  ConfigProvider.fromMap(Map("greeting" -> "Hello"))

val config: Config[String] = 
  Config.constant("Hello").orElseEither(Config.constant("Bye")).map(_.merge)
```

