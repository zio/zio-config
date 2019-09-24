package zio.config.examples

import zio.DefaultRuntime
import zio.config._

object ErrorAccumulation extends App {
  case class SampleConfig(s1: Int, s2: String)

  val config =
    (int("envvar", "tryanothervariable") <*> string("envvar2"))(SampleConfig.apply, SampleConfig.unapply)

  val runtime = new DefaultRuntime {}

  val programWithInvalidSource =
    read(config).run.provide(mapSource(Map.empty)).either

  val parsed = runtime.unsafeRun(programWithInvalidSource)

  assert(
    parsed ==
      Left(
        List(
          ConfigError(Seq("envvar"), ConfigError.MissingValue),
          ConfigError(Seq("tryanothervariable"), ConfigError.MissingValue),
          ConfigError(Seq("envvar2"), ConfigError.MissingValue)
        )
      )
  )

  val validSource = mapSource(Map("envvar" -> "1", "envvar2" -> "value"))

  val validRes = runtime.unsafeRun(read(config).run.provide(validSource))

  assert(validRes._2 == SampleConfig(1, "value"))

  val invalidSource = mapSource(Map("envvar" -> "wrong"))

  assert(
    runtime.unsafeRun(read(config).run.provide(invalidSource).either) ==
      Left(
        List(
          ConfigError(Seq("envvar"), ConfigError.ParseError("wrong", "int")),
          ConfigError(Seq("tryanothervariable"), ConfigError.MissingValue),
          ConfigError(Seq("envvar2"), ConfigError.MissingValue)
        )
      )
  )
}
