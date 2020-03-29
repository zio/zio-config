package zio.config.examples

import zio.config.ConfigDescriptor._
import zio.config._
import zio.console.Console
import zio.{ App, ZEnv, ZIO }

case class Prod(ldap: String, port: Int, dburl: Option[String])

object Prod {
  val prodConfig: ConfigDescriptor[String, String, Prod] =
    (string("LDAP") |@| int("PORT") |@|
      string("DB_URL").optional)(Prod.apply, Prod.unapply)

  val myAppLogic: ZIO[Config[Prod], Throwable, (String, Option[String])] =
    for {
      prod <- config[Prod]
    } yield (prod.ldap, prod.dburl)
}

object ReadConfig extends App {

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    for {
      console <- ZIO.environment[Console].map(_.get)
      configLayer = Config.fromMap(
        Map("LDAP" -> "ldap", "PORT" -> "1999", "DB_URL" -> "ddd"),
        Prod.prodConfig,
        "constant"
      )
      out <- Prod.myAppLogic
              .provideLayer(configLayer)
              .foldM(
                failure => console.putStrLn(failure.toString) *> ZIO.succeed(1),
                _ => ZIO.succeed(0)
              )
    } yield out
}
