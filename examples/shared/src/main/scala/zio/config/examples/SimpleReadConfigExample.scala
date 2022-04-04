package zio.config.examples

import zio._
import zio.config._
import zio.Console

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

  val configLayer = ZConfig.fromMap(
    Map("LDAP" -> "ldap", "PORT" -> "1999", "DB_URL" -> "ddd"),
    Prod.prodConfig,
    "constant"
  )

  def run =
    Prod.myAppLogic
      .provideLayer(configLayer)
      .foldZIO(
        failure => Console.printLine(failure.toString),
        _ => Console.printLine("Success")
      )
      .exitCode
}
