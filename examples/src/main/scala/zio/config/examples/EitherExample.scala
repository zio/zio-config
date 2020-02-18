package zio.config.examples

import zio.DefaultRuntime
import zio.config._
import ConfigDescriptor._
import zio.config.ReadError._
import zio.config.{ ConfigSource, _ }

object EitherExample extends App {
  final case class Ldap(value: String)  extends AnyVal
  final case class DbUrl(value: String) extends AnyVal

  case class Prod(ldap: Ldap, dburl: DbUrl)
  case class Dev(user: String, password: Int, dburl: Double)

  val prod =
    (string("x1").xmap(Ldap)(_.value) |@| string("x2").xmap(DbUrl)(_.value))(Prod.apply, Prod.unapply)

  val dev =
    (string("x3") |@| int("x4") |@| double("x5"))(Dev.apply, Dev.unapply)

  val prodOrDev =
    prod orElseEither dev

  val runtime = new DefaultRuntime {}

  val validProd =
    Map(
      "x1" -> "v1",
      "x2" -> "v2",
      "x3" -> "v3"
    )

  val source: ConfigSource[String, String] =
    ConfigSource.fromMap(validProd)

  // read(prodOrDev from source) is equivalent to Config.fromMap(prodOrDev). This is only to demonstrate that you can
  // use `from` at any point in your description, making it really flexible for the user to fetch different configs from different
  // sources.
  assert(runtime.unsafeRun(read(prodOrDev from source)) == Left(Prod(Ldap("v1"), DbUrl("v2"))))

  val validDev =
    Map(
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "2.0"
    )

  val anotherSource: ConfigSource[String, String] =
    ConfigSource.fromMap(validDev)

  assert(runtime.unsafeRun(read(prodOrDev from anotherSource)) == Right(Dev("v3", 1, 2.0)))

  val parseErrorConfig =
    Map(
      "x2" -> "v2",
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "notadouble"
    )

  val invalidSource =
    ConfigSource.fromMap(parseErrorConfig)

  assert(
    runtime.unsafeRun(read(prodOrDev from invalidSource).either) ==
      Left(
        List(
          // OrErrors indicate that either fix the error with x1 or the error with x5
          OrErrors(
            MissingValue(Vector("x1")),
            ParseError(Vector("x5"), "notadouble", "double")
          )
        )
      )
  )

  // It chooses the left, Prod
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
