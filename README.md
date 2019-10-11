# zio-config

[![CircleCI](https://circleci.com/gh/zio/zio-config/tree/master.svg?style=svg)](https://circleci.com/gh/zio/zio-config/tree/master)
[![Gitter](https://badges.gitter.im/ZIO/zio-config.svg)](https://gitter.im/ZIO/zio-config?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A [zio](https://github.com/scalaz/scalaz-zio) based  configuration parsing library.

Configuration parsing should be easy as it sounds - Hence;

 * It has no other dependencies.
 * No macros / no reflection
 * No implicits anywhere.
 * Can write the config back to key value pairs, given the same config description. 
   Allows user to generate and populate configurations in the system-env/property-files in a typesafe way from outside, ensuring successful parsing in the app.
 * Automatic report generation on the config variables.
 * Addresses nested configuration parameters
 * Automatic man pages based on the config description,  Higher level documentation for sections of config - all with a single composable syntax.
 * Can accumulate maximum errors.
 * Insanely simple to use


## Usage

```scala
package zio.config.examples

import zio.config._, Config._
import zio.console.Console
import zio.{ App, ZIO }

case class Prod(ldap: String, port: Int, dburl: Option[String])

object Prod {
  val description: ConfigDescriptor[Prod] =
    (string("LDAP") |@| 
      int("PORT") ? "Example: 8888" |@|
        string("DB_URL").optional ? "Example: abc"
        )(Prod.apply, Prod.unapply) ? "Prod Config"

  val myAppLogic: ZIO[Config[Prod] with Console, Throwable, (String, Option[String])] =
    for {
      prodConf <- config[Prod]
      written  <- ZIO.fromEither(write(description))
      report    = docs(description, Some(prodConf))
      _        <- zio.console.putStrLn(written)
      _        <- zio.console.putStrLn(report)
    } yield (prod.ldap, prod.dburl)
}

object ReadConfig extends App {

  override def run(args: List[String]): ZIO[ReadConfig.Environment, Nothing, Int] =
    Config
      .fromEnv(Prod.description)
      .flatMap(config => Prod.myAppLogic.provide(config))
      .foldM(failure => zio.console.putStrLn(failure) *> ZIO.succeed(1), _ => ZIO.succeed(0))
}
```

Please find more examples in examples module.