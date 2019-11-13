package zio.config.examples

import zio.DefaultRuntime
import zio.config._
import ConfigDescriptor._
import zio.config.ReadErrors.ReadError.{ MissingValue, ParseError }
import zio.config.{ ConfigSource, _ }

object CoproductExample extends App {
  final case class Ldap(value: String)  extends AnyVal
  final case class DbUrl(value: String) extends AnyVal

  case class Prod(ldap: Ldap, dburl: DbUrl)
  case class Dev(user: String, password: Int, dburl: Double)

  val prod =
    (string("x1").xmap(Ldap)(_.value) |@| string("x2").xmap(DbUrl)(_.value))(Prod.apply, Prod.unapply)

  val dev =
    (string("x3") |@| int("x4") |@| double("x5"))(Dev.apply, Dev.unapply)

  val prodOrDev: ConfigDescriptor[String, String, Either[Prod, Dev]] =
    prod orElseEither dev

  val runtime = new DefaultRuntime {}

  val validConfigForSampleConfig =
    Map(
      "x1" -> "v1",
      "x2" -> "v2",
      "x3" -> "v3"
    )

  val source: ConfigSource[Vector[String], String] =
    ConfigSource.fromMap(validConfigForSampleConfig)

  assert(runtime.unsafeRun(read(prodOrDev from source)) == Left(Prod(Ldap("v1"), DbUrl("v2"))))

  val validConfigForAnotherConfig =
    Map(
      "x2" -> "v2",
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "2.0"
    )

  val anotherSource: ConfigSource[Vector[String], String] =
    ConfigSource.fromMap(validConfigForAnotherConfig)

  assert(runtime.unsafeRun(read(prodOrDev from anotherSource)) == Right(Dev("v3", 1, 2.0)))

  val invalidConfig =
    Map(
      "x2" -> "v2",
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "notadouble"
    )

  val invalidSource: ConfigSource[Vector[String], String] =
    ConfigSource.fromMap(invalidConfig)

  assert(
    runtime.unsafeRun(read(prodOrDev from invalidSource).either) ==
      Left(
        ReadErrors(
          MissingValue("x1"),
          ParseError("x5", "notadouble", "double")
        )
      )
  )

  val allConfigsExist =
    Map(
      "x1" -> "v1",
      "x2" -> "v2",
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "2.0"
    )

  assert(
    runtime.unsafeRun(read(prodOrDev from ConfigSource.fromMap(allConfigsExist))) ==
      Left(Prod(Ldap("v1"), DbUrl("v2")))
  )
}
