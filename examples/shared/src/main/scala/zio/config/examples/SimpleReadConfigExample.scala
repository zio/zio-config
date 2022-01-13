package zio.config.examples

import zio.config._

import ConfigDescriptor._
import zio._

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

  def run =
    for {
      console    <- ZIO.service[Console]
      configLayer = ZConfig.fromMap(
                      Map("LDAP" -> "ldap", "PORT" -> "1999", "DB_URL" -> "ddd"),
                      Prod.prodConfig,
                      "constant"
                    )
      out        <- Prod.myAppLogic
                      .provideLayer(configLayer)
                      .foldZIO(
                        failure => console.printLine(failure.toString),
                        _ => console.printLine("Success")
                      )
                      .exitCode
    } yield out
}
