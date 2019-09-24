package zio.config.examples

import zio.{ App, ZIO }
import zio.config._

object ReadConfig extends App {
  case class Prod(ldap: String, dburl: Option[String], regions: List[Int])

  private val config =
    (string("LDAP") <*>
      opt(string("DB_URL", "DBURL")) <*>
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
        result            <- read(config).run.provide(mapSource(validConfig))
        (report, pgmConf) = result
        _ <- if (report.list == List(
                   Details("REGIONS", "1,2", "list of value of type int"),
                   Details("DB_URL", "v2", "option of value of type string"),
                   Details("LDAP", "v1", "value of type string")
                 )) ZIO.succeed(())
            else ZIO.fail(())
        configEnv <- write(config).run.provide(pgmConf)
        _ <- if (configEnv.allConfig == Map(
                   "LDAP"    -> "v1",
                   "DBURL"   -> "v2",
                   "REGIONS" -> "1,2",
                   "DB_URL"  -> "v2"
                 )) ZIO.succeed(())
            else ZIO.fail(())
      } yield ()

    appLogic.fold(_ => 1, _ => 0)
  }
}
