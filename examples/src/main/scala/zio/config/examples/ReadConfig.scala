package zio.config.examples

import zio.config._, ConfigDescriptor._
import zio.{ App, ZEnv, ZIO }

case class Prod(ldap: String, port: Int, dburl: Option[String], hello: Option[String])

object Prod {
  val prodConfig: ConfigDescriptor[String, String, Prod] =
    (string("LDAP") |@| int("PORT") |@|
      string("DB_URL").optional |@| string("hello").optional)(Prod.apply, Prod.unapply)

  val myAppLogic: ZIO[Config[Prod], Throwable, Prod] =
    for {
      prod <- config[Prod]
    } yield prod
}

object ReadConfig extends App {

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    ZIO.accessM { env =>
      Config
        .fromMap(Map("LDAP" -> "ldap", "PORT" -> "1999", "DB_UL" -> "ddd"), Prod.prodConfig)
        .flatMap(config => Prod.myAppLogic.provide(config))
        .foldM(
          failure => env.console.putStrLn(failure.toString) *> ZIO.succeed(1),
          r => ZIO.succeed(println(r)) *> ZIO.succeed(0)
        )
    }
}
