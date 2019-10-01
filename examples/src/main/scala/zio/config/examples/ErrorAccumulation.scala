package zio.config.examples

import zio.DefaultRuntime
import zio.config._

object ErrorAccumulation extends App {
  case class SampleConfig(s1: Int, s2: String)

  val config =
    (int("envvar") <*> string("envvar2"))(SampleConfig.apply, SampleConfig.unapply)

  val runtime = new DefaultRuntime {}

  val programWithInvalidSource =
    read(config).run.provide(mapSource(Map.empty)).either

  val parsed = runtime.unsafeRun(programWithInvalidSource)

  assert(
    parsed ==
      Left(
        List(
          ReadError(Seq("envvar"), ReadError.MissingValue),
          ReadError(Seq("envvar2"), ReadError.MissingValue)
        )
      )
  )

  val validSource = mapSource(Map("envvar" -> "1", "envvar2" -> "value"))

  val validRes = runtime.unsafeRun(read(config).provide(validSource))

  assert(validRes._2 == SampleConfig(1, "value"))

  val invalidSource = mapSource(Map("envvar" -> "wrong"))

  assert(
    runtime.unsafeRun(read(config).provide(invalidSource).either) ==
      Left(
        List(
          ReadError(Seq("envvar"), ReadError.ParseError("wrong", "int")),
          ReadError(Seq("envvar2"), ReadError.MissingValue)
        )
      )
  )
}
