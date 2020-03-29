package zio.config.examples

import zio.config._
import ConfigDescriptor._
import zio.config.ReadError.{ FormatError, MissingValue, OrErrors }
//import zio.config.ReadError._
import zio.config.{ ConfigSource, _ }

object EitherExample extends App {
  final case class Ldap(value: String)  extends AnyVal
  final case class DbUrl(value: String) extends AnyVal

  case class Prod(ldap: Ldap, dburl: DbUrl)
  case class Dev(user: String, password: Int, dburl: Double)

  val prod =
    (string("x1")(Ldap.apply, Ldap.unapply) |@| string("x2")(DbUrl.apply, DbUrl.unapply))(Prod.apply, Prod.unapply)

  val dev =
    (string("x3") |@| int("x4") |@| double("x5"))(Dev.apply, Dev.unapply)

  val prodOrDev =
    prod orElseEither dev

  val validProd =
    Map(
      "x1" -> "v1",
      "x2" -> "v2",
      "x3" -> "v3"
    )

  // Obviously getting a constant map source doesn't need ZIO effect
  val source: ConfigSource[String, String] =
    ConfigSource.fromMap(validProd, "constant")

  // read(prodOrDev from source) is equivalent to Config.fromMap(prodOrDev). This is only to demonstrate that you can
  // use `from` at any point in your description, making it really flexible for the user to fetch different configs from different
  // sources.
  println("normal result " + read(prodOrDev from source))
  // assert(read(prodOrDev from source) == Left(Prod(Ldap("v1"), DbUrl("v2"))))

  val validDev =
    Map(
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "2.0"
    )

  val anotherSource: ConfigSource[String, String] =
    ConfigSource.fromMap(validDev)

  println("anpther soruce result " + read(prodOrDev from anotherSource))

  //assert(read(prodOrDev from anotherSource) == Right(Dev("v3", 1, 2.0)))

  val parseErrorConfig =
    Map(
      "x2" -> "v2",
      "x3" -> "v3",
      "x4" -> "1",
      "x5" -> "notadouble"
    )

  val invalidSource =
    ConfigSource.fromMap(parseErrorConfig, "constant")

  println("errorneous exampl " + read(prodOrDev from invalidSource))

  assert(
    read(prodOrDev from invalidSource) ==
      Left(
        // OrErrors indicate that either fix the error with x1 or the error with x5
        OrErrors(
          List(
            MissingValue(Vector(Right("x1"))),
            FormatError(Vector(Right("x5")), ReadFunctions.parseErrorMessage("notadouble", "double"))
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
    read(prodOrDev from ConfigSource.fromMap(allConfigsExist)) ==
      Right(Left(Prod(Ldap("v1"), DbUrl("v2"))))
  )
}
