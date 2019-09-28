# zio-config

[![CircleCI](https://circleci.com/gh/zio/zio-config/tree/master.svg?style=svg)](https://circleci.com/gh/zio/zio-config/tree/master)
[![Gitter](https://badges.gitter.im/ZIO/zio-config.svg)](https://gitter.im/ZIO/zio-config?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

#ZIO Config
A [zio](https://github.com/scalaz/scalaz-zio) based  configuration parsing library.

Configuration parsing should be easy as it sounds - Hence;

 * It has no other dependencies.
 * No macros / no reflection
 * Can accumulate maximum errors.
 * Insanely simple to use
 * The usage pattern is going gel well with the pattern with zio environment, with `ConfigService` being available in your environment. Refer http://degoes.net/articles/zio-environment


## Usage

```scala

import zio.{App, ZIO}
import zio.config._

object ReadConfig extends App {
  case class Prod(ldap: String, dburl: Option[String], regions: List[Int])

  private val config =
    (string("LDAP") <*>
      opt(string("DB_URL")) <*>
      list(int("REGIONS")))(Prod.apply, Prod.unapply)

  // In real, this comes from environment
  private val validConfig =
    Map(
      "LDAP"    -> "v1",
      "DB_URL"  -> "v2",
      "REGIONS" -> "1,2"
    )

  override def run(args: List[String]): ZIO[ReadConfig.Environment, Nothing, Int] = {
    val appLogic =
        for {
          result <- read(config).run.provide(mapSource(validConfig))
          (report, pgmConf) = result
          _ <- if (report.list == List(
            Details("REGIONS", "1,2", "list of value of type int"),
            Details("DB_URL", "v2", "option of value of type string"),
            Details("LDAP", "v1", "value of type string")
          )) ZIO.succeed(()) else ZIO.fail(())
          configEnv <- write(config).run.provide(pgmConf)
          _ <- if (configEnv.allConfig == Map(
            "LDAP" -> "v1",
            "DB_URL"  -> "v2",
            "REGIONS" -> "1,2",
          )) ZIO.succeed(()) else ZIO.fail(())
        } yield ()

    appLogic.fold(_ => 1, _ => 0)
  }
}


```

### Examples

Find the examples in src/main/scala/zio/config/examples.

A typical pattern of our application that use zio and config library is `zio.config.examples.ProgramExample`.