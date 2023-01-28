package zio.config.examples.magnolia

import zio.config._
import zio.config.magnolia._
import zio.config.typesafe._
import zio.Config

import java.time.{LocalDate, ZonedDateTime}
import scala.util.Try
import zio.config.examples.ZioOps

import zio.ConfigProvider

object AutoDerivationCustom extends App {
  case class AppConfig(jobName: String, details: Option[Detail], s3Path: S3Path)

  case class Detail(containerId: String, executionTime: Either[ZonedDateTime, LocalDate])

  case class S3Path(s: String)

  object S3Path {
    // For some reason you decided to check if the string inside s3Path is empty or not while writing back as well
    // If this implicit doesn't exist, zio-config-magnolia falls back to its default behaviour
    // and finds out an instance for S3Path as it is a simple case class.
    implicit val descriptorOfS3Path: DeriveConfig[S3Path] =
      DeriveConfig(
        DeriveConfig[String]
          .mapOrFail(s => validateS3Path(s).toRight(Config.Error.InvalidData(message = s"Invalid s3 path: ${s}")))
      )

    private def validateS3Path(s3Path: String): Option[S3Path] =
      if (s3Path.startsWith("s3://")) Some(S3Path(s3Path)) else None
  }

  // Good to keep implicit derivations within companion objects.
  // Preferable to give descriptions to enrich error reporting of zio-config.
  object Detail

  val config =
    """
    jobName : "spark"
    s3Path  : "s3://path"
    details : {
      containerId : abcdefg
      executionTime: "2020-06-20T17:15:23.601712+10:00[Australia/Sydney]"
    }
    """

  // Custom derivation for zoned date time. Since zonedDateTime is external,
  // we couldn't have a companion object to place this implicit, and hence placed
  // globally for the automatic derivation to work.
  implicit val descriptorOfZonedDateTime: DeriveConfig[ZonedDateTime] =
    DeriveConfig(
      DeriveConfig[String]
        .mapOrFail(x =>
          Try(ZonedDateTime.parse(x)).toEither.swap.map(r => Config.Error.InvalidData(message = r.getMessage())).swap
        ) ?? "time in zoned date time"
    )

  val appConfigDesc: Config[AppConfig] =
    deriveConfig[AppConfig]

  val source: ConfigProvider = TypesafeConfigSource.fromHoconString(config)

  assert(
    read(appConfigDesc from source) equalM
      AppConfig(
        "spark",
        Some(Detail("abcdefg", Left(ZonedDateTime.parse("2020-06-20T17:15:23.601712+10:00[Australia/Sydney]")))),
        S3Path("s3://path")
      )
  )
}
