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
 * Automatic description and report generation on the config variables.
 * Can accumulate maximum errors.
 * Insanely simple to use


## Usage

```scala

import zio.{App, ZIO}
import zio.config._
import zio.console.Console

object ReadConfig extends App {
  case class Prod(ldap: String, dburl: Option[String])

  private val config =
    (string("LDAP") <*>
      opt(string("DB_URL")))(Prod.apply, Prod.unapply)

  // In real, this comes from environment
  private val validConfig =
    Map(
      "LDAP"    -> "v1",
      "DB_URL"  -> "v2",
      "REGIONS" -> "1,2"
    )
    
  val myAppLogic: ZIO[Console with ConfigSource, List[ReadError], Unit] = 
    for {
      result <- read(config).run
      (report, conf) = result
      _ <- ZIO.accessM[Console](_.console.putStrLn(report.toString))
      _ <- ZIO.accessM[Console](_.console.putStrLn(conf.toString))
      map <- write(config).run.provide(conf).either
      _ <- ZIO.accessM[Console](_.console.putStrLn(map.toString))
    } yield ()

  override def run(args: List[String]): ZIO[ReadConfig.Environment, Nothing, Int] = {
    myAppLogic
    .provide(ProgramEnv(mapSource(validConfig).configService))
    .fold(_ => 1, _ => 0)
  }
}

case class ProgramEnv(configService: ConfigSource.Service) extends ConfigSource with Console.Live

//  
// 
// Report:
//
// List(
//   Details("DB_URL", "v2", "option of value of type string"),
//   Details("LDAP", "v1", "value of type string")
// )
//
// Config:
//
//  Prod(v1, Some(v2)
//
//

```

### Examples

Find the examples in src/main/scala/zio/config/examples.

A typical pattern of our application that use zio and config library is `zio.config.examples.ProgramExample`.