package zio.config.examples.magnolia

import zio.config._
import zio.config.magnolia._
import zio.config.typesafe._

import java.time.{LocalDate, ZonedDateTime}
import scala.util.Try

object AutoDerivationCustom extends App {
  case class AppConfig(jobName: String, details: Option[Detail], s3Path: S3Path)

  case class Detail(containerId: String, executionTime: Either[ZonedDateTime, LocalDate])

  case class S3Path(s: String)

  object S3Path {
    // For some reason you decided to check if the string inside s3Path is empty or not while writing back as well
    // If this implicit doesn't exist, zio-config-magnolia falls back to its default behaviour
    // and finds out an instance for S3Path as it is a simple case class.
    implicit val descriptorOfS3Path: Descriptor[S3Path] =
      Descriptor[String]
        .transformOrFail(
          s => validateS3Path(s).toRight(s"Invalid s3 path: ${s}"),
          value => validateS3Path(value.s).map(_.s).toRight("Cannot write. Invalid S3 path.")
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
  implicit val descriptorOfZonedDateTime: Descriptor[ZonedDateTime] =
    Descriptor[String]
      .transformOrFailLeft(x => Try(ZonedDateTime.parse(x)).toEither.swap.map(_.getMessage).swap)(
        _.toString
      ) ?? "time in zoned date time"

  val appConfigDesc: ConfigDescriptor[AppConfig] =
    descriptor[AppConfig]

  val source: ConfigSource = TypesafeConfigSource.fromHoconString(config) match {
    case Right(a) => a
    case Left(_)  => throw new Exception("bad hocon string")
  }

  val s: Either[ReadError[String], AppConfig] = read(appConfigDesc from source)

  assert(
    s == Right(
      AppConfig(
        "spark",
        Some(Detail("abcdefg", Left(ZonedDateTime.parse("2020-06-20T17:15:23.601712+10:00[Australia/Sydney]")))),
        S3Path("s3://path")
      )
    )
  )
}
