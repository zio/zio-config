package zio.config.examples

import zio.config.ConfigDescriptor._
import zio.config.ReadError.{ ForceSeverity, FormatError, MissingValue, OrErrors, Step }
import zio.config._

object EitherExample extends App {
  final case class Ldap(value: String)  extends AnyVal
  final case class DbUrl(value: String) extends AnyVal

  case class Prod(ldap: Ldap, dburl: DbUrl)
  case class Dev(user: String, password: Int, dburl: Double)

  val prod =
    (string("x1")(Ldap.apply, Ldap.unapply) |@| string("x2")(
      DbUrl.apply,
      DbUrl.unapply
    ))(Prod.apply, Prod.unapply)

  val dev =
    (string("x3") |@| int("x4") |@| double("x5"))(Dev.apply, Dev.unapply)

  val prodOrDev =
    prod orElseEither dev

  val validProd =
    Map("x1" -> "v1", "x2" -> "v2", "x3" -> "v3")

  // Obviously getting a constant map source doesn't need ZIO effect
  val source: ConfigSource =
    ConfigSource.fromMap(validProd, "constant")

  val validDev =
    Map("x3" -> "v3", "x4" -> "1", "x5" -> "2.0")

  val anotherSource: ConfigSource =
    ConfigSource.fromMap(validDev)

  //assert(read(prodOrDev from anotherSource) == Right(Dev("v3", 1, 2.0)))

  val parseErrorConfig =
    Map("x2" -> "v2", "x3" -> "v3", "x4" -> "1", "x5" -> "notadouble")

  val invalidSource =
    ConfigSource.fromMap(parseErrorConfig, "constant")

  println(
    read(prodOrDev from invalidSource).swap.map(_.prettyPrint()).swap
  )

  read(prodOrDev from invalidSource).swap.map(_.prettyPrint()).swap
  /*
      ReadError:
      ╥
      ╠─MissingValue
      ║ Details: value of type string
      ║ path: x1
      ║
      ╠─FormatError
      ║ cause: Provided value is notadouble, expecting the type double
      ║ path: x5
      ▼
   */

  println(read(prodOrDev from invalidSource) )
  assert(
    read(prodOrDev from invalidSource) ==
      Left(
        // OrErrors indicate that either fix the error with x1 or the error with x5
        OrErrors(
          List(
            ForceSeverity(MissingValue(List(Step.Key("x1")), List("value of type string")), false),
            ForceSeverity(
              FormatError(
                List(Step.Key("x5")),
                parseErrorMessage("notadouble", "double")
              ),
              false
            )
          )
        )
      )
  )

  // It chooses the left, Prod
  val allConfigsExist =
    Map("x1" -> "v1", "x2" -> "v2", "x3" -> "v3", "x4" -> "1", "x5" -> "2.0")

  assert(
    read(prodOrDev from ConfigSource.fromMap(allConfigsExist)) ==
      Right(Left(Prod(Ldap("v1"), DbUrl("v2"))))
  )
}
