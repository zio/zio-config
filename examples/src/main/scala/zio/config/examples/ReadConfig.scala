package zio.config.examples

import zio.config._
import zio.console.Console
import zio.{ App, ZIO }

object ReadConfig extends App {
  case class Prod(ldap: String, dburl: Option[String])

  private val config =
    (string("LDAP") <*>
      opt(string("DB_URL")))(Prod.apply, Prod.unapply)

  // In real, this comes from environment
  private val validConfig =
    Map(
      "LDAP"   -> "v1",
      "DB_URL" -> "v2"
    )

  val myAppLogic: ZIO[Console with ConfigSource, ReadErrors, Unit] =
    ZIO.accessM(
      env =>
        for {
          result         <- read(config).run
          (report, conf) = result
          _              <- env.console.putStrLn(report.toString)
          _              <- env.console.putStrLn(conf.toString)
          map            <- write(config).run.provide(conf).either
          _              <- env.console.putStrLn(map.toString)
        } yield ()
    )

  override def run(args: List[String]): ZIO[ReadConfig.Environment, Nothing, Int] =
    myAppLogic
      .provide(ProgramEnv(mapSource(validConfig).configService))
      .fold(_ => 1, _ => 0)
}

case class ProgramEnv(configService: ConfigSource.Service) extends ConfigSource with Console.Live
