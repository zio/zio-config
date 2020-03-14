package zio.config.examples

import zio.config.ConfigDescriptor._
import zio.config.ReadError._
import zio.config._

object ErrorAccumulation extends App {
  case class SampleConfig(s1: Int, s2: String)

  val config: ConfigDescriptor[String, String, SampleConfig] =
    (int("envvar") |@| string("envvar2"))(SampleConfig.apply, SampleConfig.unapply)

  val runtime = zio.Runtime.default

  val programWithInvalidSource =
    read(config from ConfigSource.fromMap(Map.empty)).either

  val parsed = runtime.unsafeRun(programWithInvalidSource)

  assert(
    parsed ==
      Left(
        ::(
          MissingValue(Vector("envvar")),
          MissingValue(Vector("envvar2")) :: Nil
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
          ParseError(Vector("envvar"), "wrong", "int"),
          MissingValue(Vector("envvar2"))
        )
      )
  )
}
