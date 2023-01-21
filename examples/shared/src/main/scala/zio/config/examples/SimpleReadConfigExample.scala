package zio.config.examples

import zio.{Console, _}

import zio.Config
import zio.ConfigProvider
import zio.config.syntax._

final case class Prod(ldap: String, port: Int, dburl: Option[String])

object Prod {
  val prodConfig: Config[Prod] =
    (
      Config.string("LDAP") ++ Config.int("PORT") ++
        Config.string("DB_URL").optional
    ).to[Prod]
}

object ReadConfig extends ZIOAppDefault {

  val configProvider = ConfigProvider.fromMap(Map("LDAP" -> "ldap", "PORT" -> "1999", "DB_URL" -> "ddd"))

  def run: URIO[Any, ExitCode] =
    configProvider
      .load(Prod.prodConfig)
      .foldZIO(
        failure => Console.printLine(failure.toString),
        value => Console.printLine(value.toString)
      )
      .exitCode
}
