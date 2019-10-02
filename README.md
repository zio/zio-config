# zio-config

[![CircleCI](https://circleci.com/gh/zio/zio-config/tree/master.svg?style=svg)](https://circleci.com/gh/zio/zio-config/tree/master)
[![Gitter](https://badges.gitter.im/ZIO/zio-config.svg)](https://gitter.im/ZIO/zio-config?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A [zio](https://github.com/scalaz/scalaz-zio) based  configuration parsing library.

Configuration parsing should be easy as it sounds - Hence;

 * It has no other dependencies.
 * No macros / no reflection
 * No implicit requirements at user site.
 * Can write the config back to key value pairs, given the same config description. 
   Allows user to generate and populate configurations in the system-env/property-files in a typesafe way from outside, ensuring successful parsing in the app.
 * Automatic report generation on the config variables.
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
  val prodConfig: ConfigDescriptor[Prod] =
    (string("LDAP") <*> int("PORT") <*>
      string("DB_URL").optional)(Prod.apply, Prod.unapply)

  val myAppLogic: ZIO[Config[Prod], Throwable, (String, Option[String])] =
    for {
      prod    <- config[Prod]
      written <- write(Prod.prodConfig).run.provide(prod)
      _       <- ZIO.effect(println(written))
      _       <- ZIO.effect(println(userManual(Prod.prodConfig)))
    } yield (prod.ldap, prod.dburl)
}

object ReadConfig extends App {

  override def run(args: List[String]): ZIO[ReadConfig.Environment, Nothing, Int] =
    Config
      .fromEnv(Prod.prodConfig)
      .flatMap(config => Prod.myAppLogic.provide(config))
      .foldM(failure => ZIO.effectTotal(println(failure)) *> ZIO.succeed(1), _ => ZIO.succeed(0))
}

//
// Config:
//  Prod(v1, Some(v2)
//
// Report:
//   List(
//     Details("DB_URL", "v2", "option of value of type string"),
//     Details("LDAP", "v1", "value of type string")
//   )
// 
// User Manual:
//   KeyDescription(DB_URL,List(value of type string, Optional value, Db Related config))
//   KeyDescription(LDAP,List(value of type string)
//

```

Please find more examples in examples modules.