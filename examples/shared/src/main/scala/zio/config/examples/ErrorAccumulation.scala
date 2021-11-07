package zio.config.examples

import zio.config._

import ConfigDescriptor._
import ReadError._, PropertyTreePath._

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

  println(parsed.mapError(_.prettyPrint()).either.unsafeRun)
  /*
    ReadError:
    ╥
    ╠══╦══╗
    ║  ║  ║
    ║  ║  ╠─MissingValue
    ║  ║  ║ Details: value of type string
    ║  ║  ║ path: envvar2
    ║  ║  ║
    ║  ║  ╠─MissingValue
    ║  ║  ║ Details: value of type string
    ║  ║  ║ path: envvar3
    ║  ║  ▼
    ║  ║
    ║  ╠─MissingValue
    ║  ║ Details: value of type int
    ║  ║ path: envvar
    ║  ▼
    ▼
   */

  assert(
    parsed.either equalM
      Left(
        // OrErrors indicate fix either of those errors associated with envvar2 or envvar3
        // AndErrors indicate fix the errors associated with both envvar1 and OrError(envvar2 or envvar3)
        ZipErrors(
          List(
            MissingValue(List(Step.Key("envvar")), List("value of type int")),
            OrErrors(
              List(
                MissingValue(List(Step.Key("envvar2")), List("value of type string")),
                MissingValue(List(Step.Key("envvar3")), List("value of type string"))
              )
            )
          )
        )
      )
  )

  val validSource: ConfigSource =
    ConfigSource.fromMap(Map("envvar" -> "1", "envvar2" -> "value"))

  val validRes                  =
    read(config from validSource)

  assert(validRes equalM SampleConfig(1, "value"))

  val invalidSource: ConfigSource = ConfigSource.fromMap(Map("envvar" -> "wrong"))

  val result2 =
    read(config from invalidSource).mapError(_.prettyPrint())

  println(zio.Runtime.default.unsafeRun(result2.either))

  /*
  s"""
   ReadError:
   ╥
   ╠══╦══╗
   ║  ║  ║
   ║  ║  ╠─MissingValue
   ║  ║  ║ Details: value of type string
   ║  ║  ║ path: envvar2
   ║  ║  ║
   ║  ║  ╠─MissingValue
   ║  ║  ║ Details: value of type string
   ║  ║  ║ path: envvar3
   ║  ║  ▼
   ║  ║
   ║  ╠─FormatError
   ║  ║ cause: Provided value is wrong, expecting the type int
   ║  ║ path: envvar
   ║  ▼
   ▼)
   */

  assert(
    read(config from invalidSource).either equalM
      Left(
        ZipErrors(
          List(
            FormatError(
              List(Step.Key("envvar")),
              parseErrorMessage("wrong", "int"),
              List("value of type int")
            ),
            OrErrors(
              List(
                MissingValue(List(Step.Key("envvar2")), List("value of type string")),
                MissingValue(List(Step.Key("envvar3")), List("value of type string"))
              )
            )
          )
        )
      )
  )
}
