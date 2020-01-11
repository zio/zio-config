package zio.config.examples

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import zio.config.ConfigDescriptor._
import zio.config._
import zio.config.refined._
import zio.{ App, ZEnv, ZIO }

object RefinedReadConfig extends App {
  case class RefinedProd(
    ldap: Refined[String, NonEmpty],
    port: Refined[Int, GreaterEqual[W.`1024`.T]],
    dburl: Option[Refined[String, NonEmpty]],
    longs: Refined[List[Long], Size[Greater[W.`2`.T]]]
  )

  def longList(n: Int): List[ConfigDescriptor[String, String, Long]] =
    (1 to n).toList
      .map(group => long(s"GROUP${group}_LONGVAL"))

  def longs(n: Int): ConfigDescriptor[String, String, ::[Long]] =
    ConfigDescriptor.collectAll[String, String, Long](::(longList(n).head, longList(n).tail))

  def prodConfig(n: Int): ConfigDescriptor[String, String, RefinedProd] =
    (
      nonEmpty(string("LDAP")) |@|
        greaterEqual[W.`1024`.T](int("PORT")) |@|
        nonEmpty(string("DB_URL")).optional |@|
        size[Greater[W.`2`.T]](longs(n).xmap(_.toList)(list => ::(list.head, list.tail)))
    )(
      RefinedProd.apply,
      RefinedProd.unapply
    )

  val myAppLogic: ZIO[Config[RefinedProd], Nothing, Refined[List[Long], Size[Greater[W.`2`.T]]]] =
    for {
      prod <- config[RefinedProd]
    } yield prod.longs

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    ZIO.accessM { env =>
      val configMap =
        Map(
          "LDAP"           -> "ldap",
          "PORT"           -> "1999",
          "DB_URL"         -> "ddd",
          "COUNT"          -> "3",
          "GROUP1_LONGVAL" -> "1234",
          "GROUP2_LONGVAL" -> "2345",
          "GROUP3_LONGVAL" -> "3456"
        )
      val outcome: ZIO[Any, ReadErrors[Vector[String], String], Refined[List[Long], Size[Greater[W.`2`.T]]]] =
        for {
          count  <- Config.fromMap(configMap, nonNegative(int("COUNT")))
          n      = count.config.config
          config <- Config.fromMap(configMap, prodConfig(n.value))
          r      <- myAppLogic.provide(config)
        } yield r

      outcome.foldM(
        failure => env.console.putStrLn(failure.toString) *> ZIO.succeed(1),
        r => env.console.putStrLn(s"👍 $r") *> ZIO.succeed(0)
      )
    }
}
