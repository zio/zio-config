package zio.config.examples

import zio.config._
import zio.{Console, ExitCode, Has, ZEnv, ZIO, ZIOAppArgs, ZIOAppDefault}

final case class Prod(ldap: String, port: Int, dburl: Option[String])

import ConfigDescriptor._

object Prod {
  val prodConfig: ConfigDescriptor[Prod] =
    (string("LDAP") |@| int("PORT") |@|
      string("DB_URL").optional)(Prod.apply, Prod.unapply)

  val myAppLogic: ZIO[Has[Prod], Throwable, (String, Option[String])] =
    for {
      prod <- getConfig[Prod]
    } yield (prod.ldap, prod.dburl)
}

object ReadConfig extends ZIOAppDefault {

  override def run: ZIO[ZEnv with Has[ZIOAppArgs], Nothing, ExitCode] = {
    val configLayer = ZConfig.fromMap(
      Map("LDAP" -> "ldap", "PORT" -> "1999", "DB_URL" -> "ddd"),
      Prod.prodConfig,
      "constant"
    )
    for {
      out <- Prod.myAppLogic
               .provideLayer(configLayer)
               .foldZIO(
                 failure => Console.printLine(failure.toString),
                 _ => Console.printLine("Success")
               )
               .exitCode
    } yield out
  }
}
