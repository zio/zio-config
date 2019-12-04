package zio.config.examples

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import zio.config.ConfigDescriptor._
import zio.config._
import zio.config.refined._
import zio.{ App, ZEnv, ZIO }

case class RefinedProd(
  ldap: Refined[String, NonEmpty],
  port: Refined[Int, Positive],
  dburl: Option[Refined[String, NonEmpty]]
)

object RefinedProd {

  val prodConfig: ConfigDescriptor[String, String, RefinedProd] =
    ((nonEmpty(string("LDAP"))) |@| positive(int("PORT")) |@|
      nonEmpty(string("DB_URL")).optional)(RefinedProd.apply, RefinedProd.unapply)

  val myAppLogic: ZIO[Config[RefinedProd], Throwable, (Refined[String, NonEmpty], Option[Refined[String, NonEmpty]])] =
    for {
      prod <- config[RefinedProd]
    } yield (prod.ldap, prod.dburl)
}

object RefinedReadConfig extends App {
  println(RefinedProd.prodConfig)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    ZIO.accessM { env =>
      Config
        .fromMap(Map("LDAP" -> "ldap", "PORT" -> "1999", "DB_URL" -> "ddd"), RefinedProd.prodConfig)
        .flatMap(config => RefinedProd.myAppLogic.provide(config))
        .foldM(failure => env.console.putStrLn(failure.toString) *> ZIO.succeed(1), _ => ZIO.succeed(0))
    }
}
