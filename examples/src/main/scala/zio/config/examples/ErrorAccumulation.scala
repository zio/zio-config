package zio.config.examples

import zio.config._
import zio.config.ConfigDescriptor._
import zio.config.ReadError._

object ErrorAccumulation extends App {
  case class SampleConfig(s1: Int, s2: String)

  val config: ConfigDescriptor[SampleConfig] =
    (int("envvar") |@| string("envvar2").orElse(string("envvar3")))(
      SampleConfig.apply,
      SampleConfig.unapply
    )

  val runtime = zio.Runtime.default

  val parsed =
    read(config from ConfigSource.fromMap(Map.empty))

  assert(
    parsed ==
      Left(
        // OrErrors indicate fix either of those errors associated with envvar2 or envvar3
        // AndErrors indicate fix the errors associated with both envvar1 and OrError(envvar2 or envvar3)
        AndErrors(
          List(
            MissingValue(List(Step.Key("envvar"))),
            OrErrors(
              List(
                MissingValue(List(Step.Key("envvar2"))),
                MissingValue(List(Step.Key("envvar3")))
              )
            )
          )
        )
      )
  )

  val validSource =
    ConfigSource.fromMap(Map("envvar" -> "1", "envvar2" -> "value"))

  val validRes = read(config from validSource)

  assert(validRes == Right(SampleConfig(1, "value")))

  val invalidSource = ConfigSource.fromMap(Map("envvar" -> "wrong"))

  assert(
    read(config from invalidSource) ==
      Left(
        AndErrors(
          List(
            FormatError(
              List(Step.Key("envvar")),
              parseErrorMessage("wrong", "int")
            ),
            OrErrors(
              List(
                MissingValue(List(Step.Key("envvar2"))),
                MissingValue(List(Step.Key("envvar3")))
              )
            )
          )
        )
      )
  )
}
