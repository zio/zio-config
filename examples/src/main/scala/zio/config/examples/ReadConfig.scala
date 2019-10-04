package zio.config.examples

import zio.config._, Config._
import zio.{ App, ZIO }

case class Prod(ldap: String, port: Int, dburl: Option[String])

object Prod {
  val prodConfig: ConfigDescriptor[Prod] =
    (string("LDAP") |@| int("PORT") |@|
      string("DB_URL").optional)(Prod.apply, Prod.unapply)

  val myAppLogic: ZIO[Config[Prod], Throwable, (String, Option[String])] =
    for {
      prod <- config[Prod]
    } yield (prod.ldap, prod.dburl)
}

object ReadConfig extends App {

  override def run(args: List[String]): ZIO[ReadConfig.Environment, Nothing, Int] =
    ZIO.accessM(env => {
      Config
        .fromEnv(Prod.prodConfig)
        .flatMap(config => Prod.myAppLogic.provide(config))
        .foldM(failure => env.console.putStrLn(failure.toString) *> ZIO.succeed(1), _ => ZIO.succeed(0))
    })
}
