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

  val programWithInvalidSource =
    read(config from ConfigSource.fromMap(Map.empty)).either

  val parsed = runtime.unsafeRun(programWithInvalidSource)

  assert(
    parsed ==
      Left(
        List(
          // Unlike OrErrors, AndErrors indicate fix the errors associated with both envvar1 and envvar2
          // Unlike AndErrors, OrErrors indicate fix either of those errors associated with envvar2 or envvar3
          // Some of the type inference issues are only applicable in scala 2.12
          AndErrors(
            ::(
              MissingValue(Vector("envvar")),
              ::(
                OrErrors(MissingValue(Vector("envvar2")), MissingValue(Vector("envvar3"))): ReadError[
                  Vector[String],
                  String
                ],
                Nil
              )
            )
          )
        )
      )
  )

  val validSource = ConfigSource.fromMap(Map("envvar" -> "1", "envvar2" -> "value"))

  val validRes = runtime.unsafeRun(read(config from validSource))

  assert(validRes == SampleConfig(1, "value"))

  val invalidSource = ConfigSource.fromMap(Map("envvar" -> "wrong"))

  assert(
    runtime.unsafeRun(read(config from invalidSource).either) ==
      Left(
        List(
          // Unlike OrErrors, AndErrors indicate fix the errors associated with both envvar1 and envvar2
          AndErrors(
            ::(
              ParseError(Vector("envvar"), "wrong", "int"): ReadError[Vector[String], String],
              ::(
                OrErrors(MissingValue(Vector("envvar2")), MissingValue(Vector("envvar3"))): ReadError[
                  Vector[String],
                  String
                ],
                Nil
              )
            )
          )
        )
      )
  )
}
