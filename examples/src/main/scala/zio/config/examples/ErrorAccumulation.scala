package zio.config.examples

import zio.DefaultRuntime
import zio.config._, ConfigDescriptor._
import zio.config.ReadError._
import zio.config._

object ErrorAccumulation extends App {
  case class SampleConfig(s1: Int, s2: String)

  val config: ConfigDescriptor[String, String, SampleConfig] =
    (int("envvar") |@| string("envvar2").orElse(string("envvar3")))(SampleConfig.apply, SampleConfig.unapply)

  val runtime = new DefaultRuntime {}

  val parsed =
    read(config from ConfigSource.fromMap(Map.empty))

  println(parsed)

  assert(
    parsed ==
      Left(
        // OrErrors indicate fix either of those errors associated with envvar2 or envvar3
        // AndErrors indicate fix the errors associated with both envvar1 and OrError(envvar2 or envvar3)
        AndErrors(
          MissingValue(Vector("envvar").toString),
          OrErrors(MissingValue(Vector("envvar2").toString), MissingValue(Vector("envvar3").toString))
        )
      )
  )

  val validSource = ConfigSource.fromMap(Map("envvar" -> "1", "envvar2" -> "value"))

  val validRes = read(config from validSource)

  assert(validRes == Right(SampleConfig(1, "value")))

  val invalidSource = ConfigSource.fromMap(Map("envvar" -> "wrong"))

  assert(
    read(config from invalidSource) ==
      Left(
        AndErrors(
          ParseError(Vector("envvar").toString, ReadFunctions.parseErrorMessage("wrong", "int")),
          OrErrors(MissingValue(Vector("envvar2").toString), MissingValue(Vector("envvar3").toString)): ReadError
        )
      )
  )
}
