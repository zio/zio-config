package zio.config.examples.refined

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.{ NonEmpty, Size }
import eu.timepit.refined.numeric.{ Greater, GreaterEqual }
import zio.config.ConfigDescriptor.{ int, list, long, string }
import zio.config.refined.{ greaterEqual, nonEmpty, size }
import zio.config.{ read, ConfigDescriptor, ConfigSource, ReadErrors }
import zio.{ console, App, ZEnv, ZIO }

object RefinedReadConfig extends App {

  case class RefinedProd(
    ldap: Refined[String, NonEmpty],
    port: Refined[Int, GreaterEqual[W.`1024`.T]],
    dburl: Option[Refined[String, NonEmpty]],
    longs: Refined[List[Long], Size[Greater[W.`2`.T]]]
  )

  def prodConfig: ConfigDescriptor[String, String, RefinedProd] =
    (
      nonEmpty(string("LDAP")) |@|
        greaterEqual[W.`1024`.T](int("PORT")) |@|
        nonEmpty(string("DB_URL")).optional |@|
        size[Greater[W.`2`.T]](list(long("LONGVALS")))
    )(
      RefinedProd.apply,
      RefinedProd.unapply
    )

  val myAppLogic: ZIO[RefinedProd, Nothing, Refined[List[Long], Size[Greater[W.`2`.T]]]] =
    ZIO.access[RefinedProd](_.longs)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val configMultiMap =
      Map(
        "LDAP"     -> ::("ldap", Nil),
        "PORT"     -> ::("1999", Nil),
        "DB_URL"   -> ::("ddd", Nil),
        "LONGVALS" -> ::("1234", List("2345", "3456"))
      )

    val outcome: ZIO[Any, ReadErrors[Vector[String], String], Refined[List[Long], Size[Greater[W.`2`.T]]]] =
      for {
        config <- read(prodConfig.from(ConfigSource.fromMultiMap(configMultiMap)))
        r      <- myAppLogic.provide(config)
      } yield r

    outcome.foldM(
      failure => console.putStrLn(failure.toString) *> ZIO.succeed(1),
      r => console.putStrLn(s"👍 $r") *> ZIO.succeed(0)
    )
  }
}
