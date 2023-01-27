package zio.config.examples.refined

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.{NonEmpty, Size}
import eu.timepit.refined.numeric.{Greater, GreaterEqual}
import zio.config._
import zio.config.refined._

import Config._

object RefinedReadConfig extends App {
  case class RefinedProd(
    ldap: Refined[String, NonEmpty],
    port: Refined[Int, GreaterEqual[W.`1024`.T]],
    dbUrl: Option[Refined[String, NonEmpty]],
    longs: Refined[List[Long], Size[Greater[W.`2`.T]]]
  )

  def prodConfig =
    (
      refine[String, NonEmpty]("LDAP") zip
        refine[GreaterEqual[W.`1024`.T]](int("PORT")) zip
        refine[String, NonEmpty]("DB_URL").optional zip
        refine[Size[Greater[W.`2`.T]]](listOf("LONGS")(long))
    ).to[RefinedProd]

  val configMultiMap: Map[String, ::[String]] =
    Map(
      "LDAP"   -> ::("ldap", Nil),
      "PORT"   -> ::("1999", Nil),
      "DB_URL" -> ::("ddd", Nil),
      "LONGS"  -> ::("1234", List("2345", "3456"))
    )

  read(prodConfig.from(ConfigSource.fromMultiMap(configMultiMap))).unsafeRun
  // Right(RefinedProd(ldap,1999,Some(ddd),List(1234, 2345, 3456)))

  // you can also derive the descriptor automatically

  import zio.config.magnolia.descriptor

  val prodConfigAutomatic =
    read(
      descriptor[RefinedProd].mapKey(toSnakeCase).mapKey(_.toUpperCase) from ConfigSource.fromMultiMap(configMultiMap)
    )

  // Right(RefinedProd(ldap,1999,Some(ddd),List(1234, 2345, 3456)))

}
