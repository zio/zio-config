package zio.config.examples

import zio.{ App, ZIO }
import zio.config._

object ReadConfig extends App {

  final case class Ldap(value: String)  extends AnyVal
  final case class DbUrl(value: String) extends AnyVal

  object DbUrl {
    def fromString(s: String): Either[ReadError, DbUrl] =
      Right(DbUrl(s))
  }

  case class Prod(ldap: Ldap, dburl: Option[DbUrl], regions: List[Int])

  private val config =
    (string("LDAP").onError(_ => "v1").xmap(Ldap)(_.value) <*>
      opt(string("DB_URL", "DBURL").errorMap(DbUrl.fromString)(value => Right(value.value))) <*>
      list(int("REGIONS")))(Prod.apply, Prod.unapply)

  // In real, this comes from environment
  private val validConfig =
    Map(
      "DB_URL"  -> "v2",
      "REGIONS" -> "1,2,3,4"
    )

  override def run(args: List[String]): ZIO[ReadConfig.Environment, Nothing, Int] =
    ZIO.accessM { env =>
      val appLogic =
        for {
          result            <- read(config).run.provide(mapSource(validConfig))
          (report, pgmConf) = result
          _                 <- env.console.putStrLn(report.toString)
          configEnv         <- write(config).run.provide(pgmConf)
          _                 <- env.console.putStrLn(configEnv.toString)
        } yield ()

      appLogic.fold(_ => 1, _ => 0)
    }
}
