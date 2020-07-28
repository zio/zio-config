package zio.config.examples

import zio.config._, ConfigDescriptor._, ReadError._

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

  println(parsed.swap.map(_.prettyPrint()))
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
    parsed ==
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

  val validSource =
    ConfigSource.fromMap(Map("envvar" -> "1", "envvar2" -> "value"))

  val validRes = read(config from validSource)

  assert(validRes == Right(SampleConfig(1, "value")))

  val invalidSource = ConfigSource.fromMap(Map("envvar" -> "wrong"))

  val result2 =
    read(config from invalidSource)

  println(result2.swap.map(_.prettyPrint()))

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
    read(config from invalidSource) ==
      Left(
        ZipErrors(
          List(
            FormatError(
              List(Step.Key("envvar")),
              parseErrorMessage("wrong", "int")
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
