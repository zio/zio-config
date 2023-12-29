package zio.config.examples

import zio.config._
import zio.{Config, ConfigProvider, Console, _}

import scala.annotation.nowarn

final case class Prod(ldap: String, port: Int, dburl: Option[String])

object Prod {
  val prodConfig: Config[Prod] =
    (
      Config.string("LDAP") ++ Config.int("PORT") ++
        Config.string("DB_URL").optional
    ).to[Prod]
}

object ReadConfig extends ZIOAppDefault {

  private val configProvider: ConfigProvider =
    ConfigProvider.fromMap(Map("LDAP" -> "ldap", "PORT" -> "1999", "DB_URL" -> "ddd"))

  @nowarn("cat=lint-infer-any")
  def run: URIO[Any, ExitCode] =
    read(Prod.prodConfig from configProvider)
      .foldZIO(
        failure => Console.printLine(failure.toString),
        value => Console.printLine(value.toString)
      )
      .exitCode
}
