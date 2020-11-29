package zio.config.examples

import zio.config._
import ConfigDescriptor._
import zio.config.magnolia.DeriveConfigDescriptor.Descriptor

object RandomConfigGenerationComplexExample extends App {
  sealed trait VersionStrategy

  object VersionStrategy {
    case object Latest        extends VersionStrategy
    case class Number(n: Int) extends VersionStrategy
  }

  final case class VersionInfo(key: String, strategy: VersionStrategy)

  object VersionInfo {
    val versionInfoMap: ConfigDescriptor[Map[String, Either[Int, String]]] =
      map("versionInfo")(int.orElseEither(string))

    // A custom config descriptor for VersionInfo.
    // If we were offloading this task to zio-config-magnolia, then user need to feed in a verbose
    // config to satisfy VersionInfo, such as `versionInfo: { key :.. strategy: { number : ... }}`
    // instead of a simple `versionInfo : { version : 1 }`
    implicit val descriptorOfVersionInfo: Descriptor[VersionInfo] = {
      Descriptor(
        versionInfoMap.transformOrFail[VersionInfo](
          descMap =>
            descMap.headOption match {
              case Some((k, v)) =>
                v match {
                  case Left(value)                       => Right(VersionInfo(k, VersionStrategy.Number(value)))
                  case Right(value) if value == "latest" => Right(VersionInfo(k, VersionStrategy.Latest))
                  case Right(_) =>
                    Left("The value of a version can be either latest or a version number of the type Int")
                }
              case None => Left("Empty version info")
            },
          verionInfo =>
            verionInfo.strategy match {
              case VersionStrategy.Latest    => Right(Map(verionInfo.key -> Right("latest")))
              case VersionStrategy.Number(n) => Right(Map(verionInfo.key -> Left(n)))
            }
        )
      )
    }
  }

  final case class Database(host: java.net.URL, port: Int)
  final case class MyConfig(database: Database, versionInfo: VersionInfo, inputDir: String)

  import zio.config.magnolia._, zio.config.gen._

  // Since we have custom configuration, its difficult for user to figure out the
  // format of config and possible values. Hence we use zio.config.gen that emits
  // a sample config given a ConfigDescriptor
  println(generateConfigJson(descriptor[MyConfig]).unsafeRunChunk)

  /**
 * yields:
 *
 * {{{
 *   Chunk({
 *     "database" : {
 *         "host" : "http://def",
 *         "port" : "9793"
 *     },
 *     "inputDir" : "CZ4ckWoJKXfgMzq42rUU",
 *     "versionInfo" : {
 *         "versionInfo" : {
 *             "peMV1u1GOX1HcpRYNFx" : "latest"
 *         }
 *     }
 * }
 * )
 * }}}
 */
}
