package zio.config.examples.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import zio.config._
import zio.config.examples.ZioOps
import zio.config.refined._
import zio.{Config, ConfigProvider, IO}

object RefinedReadConfig extends App {
  case class RefinedProd(
    ldap: Refined[String, NonEmpty],
    dbUrl: Option[Refined[String, NonEmpty]]
  )

  def prodConfig: Config[RefinedProd] =
    (
      refine[String, NonEmpty]("LDAP") zip
        refine[String, NonEmpty]("DB_URL").optional
    ).to[RefinedProd]

  val configMap: Map[String, String] =
    Map(
      "LDAP"   -> "ldap",
      "DB_URL" -> "ddd"
    )

  read(prodConfig.from(ConfigProvider.fromMap(configMap))).unsafeRun

  // you can also derive the descriptor automatically

  import zio.config.magnolia.deriveConfig

  val prodConfigAutomatic: IO[Config.Error, RefinedProd] =
    read(
      deriveConfig[RefinedProd].mapKey(toSnakeCase).mapKey(_.toUpperCase) from ConfigProvider.fromMap(
        configMap
      )
    )

}
