---
id: quickstart_index
title:  "Quick Start"
---


The library aims to have a powerful & purely functional, yet a thin interface to access configuration information inside an application.
There are many examples in [here](https://github.com/zio/zio-config/tree/master/examples/src/main/scala/zio/config/examples) straight away as well.

__Note that this documentation is for 1.x series. For newer versions, please refer to [docs](https://github.com/zio/zio-config/tree/master/docs) section in GitHub.__

## Adding dependency

If you are using sbt:

```scala

libraryDependencies += "dev.zio" %% "zio-config" % <version>

```

##### Optional Dependency with magnolia module (Auto derivation)

```scala

libraryDependencies += "dev.zio" %% "zio-config-magnolia" % <version>

```

##### Optional Dependency with refined module (Integration with refined library)

```scala

libraryDependencies += "dev.zio" %% "zio-config-refined" % <version>

```


##### Optional Dependency with typesafe module (HOCON/Json source)

```scala

libraryDependencies += "dev.zio" %% "zio-config-typesafe" % <version>

```


##### Optional Dependency with yaml module (Yaml source)

```scala

libraryDependencies += "dev.zio" %% "zio-config-yaml" % <version>

```

##### Optional Dependency for a random generation of a config

```scala

libraryDependencies += "dev.zio" %% "zio-config-gen" % <version>

```

## Describe the config by hand

We must fetch the configuration from the environment to a case class (product) in scala. Let it be `MyConfig`


```scala mdoc:silent
import zio.IO

import zio.config._, ConfigDescriptor._, ConfigSource._

```

```scala mdoc:silent

case class MyConfig(ldap: String, port: Int, dburl: String)

```
To perform any action using zio-config, we need a configuration description.
Let's define a simple one.


```scala mdoc:silent

val myConfig: ConfigDescriptor[MyConfig] =
  (string("LDAP") zip int("PORT") zip string("DB_URL")).to[MyConfig]

 // ConfigDescriptor[ MyConfig]

```

To get a tuple,

```scala mdoc:silent

val myConfigTupled: ConfigDescriptor[(String, Int, String)] =
  (string("LDAP") zip int("PORT") zip string("DB_URL"))


```

## Fully automated Config Description

If you don't like describing your configuration manually, and rely on the names of the parameter in the case class (or sealed trait),
there is a separate module called `zio-config-magnolia`. 

Note:  `zio-config-shapeless` is an alternative to `zio-config-magnolia` to support scala 2.11 projects. 
It will be deprecated once we find users have moved on from scala 2.11. 


```scala mdoc:silent

import zio.config._
import zio.config.magnolia.{Descriptor, descriptor}

val myConfigAutomatic = descriptor[MyConfig]

```

`myConfig` and `myConfigAutomatic` are same description, and is of the same type. 

Refer to API docs for more explanations on [descriptor](https://javadoc.io/static/dev.zio/zio-config-magnolia_2.13/1.0.0-RC31-1/zio/config/magnolia/index.html#descriptor[A](implicitconfig:zio.config.magnolia.package.Descriptor[A]):zio.config.ConfigDescriptor[A])
More examples on automatic derivation is in examples module of [zio-config](https://github.com/zio/zio-config)

## Read config from various sources

There are more information on various sources in [here](../sources/index.md).

Below given is a simple example.

```scala mdoc:silent
val map =
  Map(
    "LDAP" -> "xyz",
    "PORT" -> "8888",
    "DB_URL" -> "postgres"
  )

val source = ConfigSource.fromMap(map)

read(myConfig from source)

// Alternatively, you can rely on `Config.from..` pattern to get ZLayers.
val result =
  ZConfig.fromMap(map, myConfig)

// Layer[ReadError[String], Config[A]]  

```

You can run this to [completion](https://zio.dev/docs/getting_started.html#main) as in any zio application.

## How to use config descriptor

### Readers from configdescriptor

As mentioned before, you can use config descriptor to read from various sources.

```scala mdoc:silent

val anotherResult =
  read(myConfig from source)
```

Note that, this is almost similar to `Config.fromMap(map, myConfig)` in the previous section.

More details in [here](../configdescriptor/index.md).

### Documentations using configdescriptor

```scala mdoc:silent
generateDocs(myConfig)
//Creates documentation (automatic)

val betterConfig =
  (string("LDAP") ?? "Related to auth" zip int("PORT") ?? "Database port" zip
    string("DB_URL") ?? "url of database"
   ).to[MyConfig]

generateDocs(betterConfig).toTable.toGithubFlavouredMarkdown
// Custom documentation along with auto generated docs
```

More details in [here](../configdescriptor/index.md).


### Writers from configdescriptor

```scala mdoc:silent

write(myConfig, MyConfig("xyz", 8888, "postgres")).map(_.flattenString())
//  Map("LDAP" -> "xyz", "PORT" -> "8888", "DB_URL" -> "postgres")

```

More details in [here](../configdescriptor/index.md).

### Report generation from configdescriptor


```scala mdoc:silent
generateReport(myConfig, MyConfig("xyz", 8888, "postgres"))
// Generates documentation showing value of each parameter

```

#### Generate a random config

```scala mdoc:silent

import zio.config.derivation.name
import zio.config.magnolia._, zio.config.gen._

object RandomConfigGenerationSimpleExample extends App {
  sealed trait Region

  @name("ap-southeast-2")
  case object ApSouthEast2 extends Region

  @name("us-east")
  case object UsEast extends Region

  case class DetailsConfig(username: String, region: Region)

  println(generateConfigJson(descriptor[DetailsConfig]).unsafeRunChunk)

  // yields for example

  // Chunk(
  //   {
  //    "region" : "ap-southeast-2",
  //     "username" : "eU2KlfATwYZ5s0Y"
  //   }
  // )
}


```
### Accumulating all errors

For any misconfiguration, the ReadError collects all of them with proper semantics: `AndErrors` and `OrErrors`. 
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

It says, fix `FormatError` related to path "var1" in the source. For the next error, either provide var2 or var3
to fix `MissingValue` error.

**Note**: Use prettyPrint method to avoid having to avoid seeing highly nested ReadErrors, that can be difficult to read.

## Config is your ZIO environment

Take a look at the below example that shows an entire mini app.
This will tell you how to consider configuration as just a part of `Environment` of your ZIO functions across your application.

```scala mdoc:silent

import zio._

case class ApplicationConfig(bridgeIp: String, userName: String)

val configuration =
  (string("bridgeIp") zip string("username")).to[ApplicationConfig]

val finalExecution =
  for {
    appConfig <- getConfig[ApplicationConfig]
    _         <- Console.printLine(appConfig.bridgeIp)
    _         <- Console.printLine(appConfig.userName)
  } yield ()

val configLayer = ZConfig.fromPropertiesFile("file-location", configuration)

// Main App
val pgm = finalExecution.provideLayer(configLayer)

```

### Separation of concerns with `narrow`

In bigger apps you can have a lot of components and, consequently - a lot of configuration fields. It's not ideal to pass around the whole configuration object as a dependency for all of those components: this way you break the separation of concerns principle. Component should be aware only about dependencies it cares about and uses somehow.

So to avoid that and do it in an ergonomic way, there's a `narrow` syntax extension for `ZLayer`, available under `import zio.config.syntax._` import:

```scala mdoc:silent
import zio._
import zio.config.typesafe._
import zio.config.syntax._
import zio.config.magnolia._

trait Endpoint
trait Repository

case class AppConfig(api: ApiConfig, db: DbConfig)
case class DbConfig (url: String,    driver: String)
case class ApiConfig(host: String,   port: Int)

val configDescription = descriptor[AppConfig]

// components have only required dependencies
val endpoint: ZLayer[ApiConfig, Nothing, Endpoint]    = ZLayer.succeed(new Endpoint {})
val repository: ZLayer[DbConfig, Nothing, Repository] = ZLayer.succeed(new Repository {}) 

val cfg = TypesafeConfig.fromResourcePath(configDescription)

cfg.narrow(_.api) >>> endpoint // narrowing down to a proper config subtree
cfg.narrow(_.db) >>> repository
```


### What's new in zio-config-3.x ?

Some of these details are repeated in certain parts of the documentations.
We thought we will repeat here, which is much better than readers missing it out.

## Removed `DeriveConfigDescriptor` and `SealedTraitStrategy`

`DeriveConfigDescriptor` used to be the interface where users can override certain default behaviours of automatic derivation, mainly to change the way zio-config handles custom key names, and coproducts (sealed traits). Now this is deleted forever.

Take a look at the API docs of `descriptor`, `descriptorForPureConfig`, `descriptorWithClassNames` and `descriptorWithoutClassNames` for more information.

https://github.com/zio/zio-config/blob/master/magnolia/shared/src/main/scala-2.12-2.13/zio/config/magnolia/package.scala


## Custom keys is just about changing `ConfigDescriptor`

We recommend users to make use of `mapKey` in `ConfigDescriptor` to change any behaviour of the field-names (or class names, or sealed-trait names). The release ensures we no longer need to extend an interface called `DeriveConfigDescriptor` to change this behaviour.

### Example:

Now on, the only way to change keys is as follows:

```scala
  // mapKey is just a function in `ConfigDescriptor` that pre-existed

  val config = descriptor[Config].mapKey(_.toUpperCase)
 
```

instead of 

```scala

// No longer supported
val customDerivation = new DeriveConfigDescriptor {
  override def mapFieldName(key: String) = key.toUpperCase
 }

import customDerivation._

val config = descriptor[Config]

```

## Inbuilt support for pure-config

Many users make use of the label `type` in HOCON files to annotate the type of the coproduct.
Now on, zio-config has inbuilt support for reading such a file/string using `descriptorForPureConfig`.


```scala

    import zio.config._, typesafe._, magnolia._

     sealed trait X
     case class A(name: String) extends X
     case class B(age: Int) extends X

     case class AppConfig(x: X)

     val str =
       s"""
        x : {
          type = A
          name = jon
        }
       """

     read(descriptorForPureConfig[AppConfig] from ConfigSource.fromHoconString(str))


```

## Allow concise config source strings

With this release we have `descriptorWithoutClassNames` along with `descriptor` that just completely discards the name of the sealed-trait and sub-class (case-class) names, allowing your source less verbose. Note that unlike `pure-config` example above, we don't need to have an extra label `type : A`.

```scala

 sealed trait Y

 object Y {
   case class A(age: Int)     extends Y
   case class B(name: String) extends Y
 }

 case class AppConfig(x: Y)

 val str =
        s"""
            x : {
              age : 10
            }
           """

   read(descriptorWithoutClassNames[AppConfig] from ConfigSource.fromHoconString(str))
```

PS: If you are using `descriptor` instead of `descriptorWithoutClassNames`, then the source has to be:

```scala

 
            x : {
              A : { 
                  age : 10
              }
            }


```


## Your ConfigSource is exactly your product and coproduct

Some users prefer to encode the config-source exactly the same as that of Scala class files. The implication is, the source will know the name of the `sealed trait` and the name of all of its `subclasses`. There are several advantages to such an approach, while it can be questionable in certain situations. Regardless, zio-config now has inbuilt support to have this pattern.

### Example: 

Say, the config ADT is as below:

```scala
   sealed trait Y

   object Y {
     case class A(age: Int)     extends Y
     case class B(name: String) extends Y
   }

   case class AppConfig(x: X)



```

Then the corresponding config-source should be as follows. Keep a note that under `x`, the name of sealed trait `Y` also exist.


```scala

      val str =
        s"""
           x : {
                 Y : {
                    A : {
                      age : 10
                    }
               }
           }
          """

```


To read such a string (or any config-source encoded in such a hierarchy), use `descriptorWithClassNames` instead of `descriptor`. In short, `descriptorWithClassNames` considers the name of sealed-trait.


```scala

read(descriptorWithClassNames[AppConfig] from ConfigSource.fromHoconString(str))


```

## More composable `Descriptor`

The whole bunch of methods such as `descriptor` works with the type class `Descriptor`. You can summon a `Descriptor` for type `A` using `Descriptor[A].apply`, which will give you access to lower level methods such as `removeSubClassNameKey`. These methods directly exist in `ConfigDescriptor`, however inaccessible, since there is no guarantee that a manually created `ConfigDescriptor` correctly tags keys to its types (i.e, a particular key is the name of a sub-class of a sealed-trait)

```scala
  case class A (...)

  val config1: Descriptor[A] =  Descriptor[A].removeSealedTraitNameKey
  val config2: Descriptor[A] = Descriptor[A].removeSubClassNameKey

 // similar to descriptorWithoutClassNames 
  val config3: Descriptor[A] =  Descriptor[A].removeSealedTraitNameKey. removeSubClassNameKey.mapFieldName(_.toUpperCase) 

  

```
