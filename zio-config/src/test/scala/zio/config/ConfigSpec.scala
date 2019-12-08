package zio.config

import zio.IO
import zio.config.ConfigDescriptor._
import zio.config.ConfigSpecUtils._
import zio.config.ReadError.ParseError
import zio.test.Assertion._
import zio.test._

object ConfigSpec
    extends DefaultRunnableSpec(
      suite("config suite")(
        testM("optional missing Int example") {
          val envVars: Map[String, String] = Map("DOMAIN" -> "dom1")
          for {
            c        <- readConfigM(envVars)
            internal <- c.config.config
          } yield assert(internal, equalTo(ConfigInternal("dom1", None)))
        },
        testM("optional Int wrong type example") {
          val envVars: Map[String, String] = Map(
            "DOMAIN" -> "dom1",
            "PORT"   -> "NOT_AN_INT"
          )
          assertM(
            readConfigM(envVars).either,
            isLeft(equalTo(singleton(ParseError(Vector("PORT"), "NOT_AN_INT", "int"))))
          )
        }
      )
    )

object ConfigSpecUtils {
  final case class ConfigInternal private (domain: String, maybePort: Option[Int])

  private val configDescriptor: ConfigDescriptor[String, String, ConfigInternal] =
    (string("DOMAIN") |@| int("PORT").optional)(ConfigInternal.apply, ConfigInternal.unapply)

  def readConfigM(envVars: Map[String, String]): IO[ReadErrors[Vector[String], String], Config[ConfigInternal]] =
    zio.config.Config.fromMap(envVars, configDescriptor)
}
