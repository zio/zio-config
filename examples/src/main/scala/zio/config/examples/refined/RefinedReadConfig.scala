package zio.config.examples.refined

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.{ NonEmpty, Size }
import eu.timepit.refined.numeric.{ Greater, GreaterEqual }
import zio.config.refined._
import zio.config._, ConfigDescriptor._

object RefinedReadConfig extends App {
  case class RefinedProd(
    ldap: Refined[String, NonEmpty],
    port: Refined[Int, GreaterEqual[W.`1024`.T]],
    dburl: Option[Refined[String, NonEmpty]],
    longs: Refined[List[Long], Size[Greater[W.`2`.T]]]
  )

  def prodConfig =
    (
      nonEmpty(string("LDAP")) |@|
        greaterEqual[W.`1024`.T](int("PORT")) |@|
        nonEmpty(string("DB_URL")).optional |@|
        size[Greater[W.`2`.T]](list("LONGVALS")(long))
    )(
      RefinedProd.apply,
      RefinedProd.unapply
    )
  val configMultiMap =
    Map(
      "LDAP"     -> ::("ldap", Nil),
      "PORT"     -> ::("1999", Nil),
      "DB_URL"   -> ::("ddd", Nil),
      "LONGVALS" -> ::("1234", List("2345", "3456"))
    )

  read(prodConfig.from(ConfigSource.fromMultiMap(configMultiMap)))
  // Right(RefinedProd(ldap,1999,Some(ddd),List(1234, 2345, 3456)))
}
