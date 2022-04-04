package zio.config.examples

import zio.config._
import zio.{Console, _}

import ConfigDescriptor._

final case class Prod(ldap: String, port: Int, dburl: Option[String])

object Prod {
  val prodConfig: ConfigDescriptor[Prod] =
    (string("LDAP") zip int("PORT") zip
      string("DB_URL").optional).to[Prod]

  val myAppLogic: ZIO[Prod, Throwable, (String, Option[String])] =
    for {
      prod <- getConfig[Prod]
    } yield (prod.ldap, prod.dburl)
}

object ReadConfig extends ZIOAppDefault {

  val configLayer: Layer[ReadError[String],Prod] = ZConfig.fromMap(
    Map("LDAP" -> "ldap", "PORT" -> "1999", "DB_URL" -> "ddd"),
    Prod.prodConfig,
    "constant"
  )

  def run: URIO[Any,ExitCode] =
    Prod.myAppLogic
      .provideLayer(configLayer)
      .foldZIO(
        failure => Console.printLine(failure.toString),
        _ => Console.printLine("Success")
      )
      .exitCode
}
