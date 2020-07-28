package zio.config.examples

import zio.config._, ConfigDescriptor._

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
  /*
      ╥
      ╠══╗
      ║  ║
      ║  ╠─MissingValue
      ║  ║ path: x1
      ║  ║ Details: value of type string
      ║  ▼
      ║
      ╠══╗
      ║  ║
      ║  ╠─FormatError
      ║  ║ cause: Provided value is notadouble, expecting the type double
      ║  ║ path: x5
      ║  ▼
      ▼
   */

  // Please refer to accumulated errors section in quick start page to know more about the structure of errors.
  // In the above case, it means, fix either x1 or x5.

  // It chooses the left, Prod
  val allConfigsExist =
    Map("x1" -> "v1", "x2" -> "v2", "x3" -> "v3", "x4" -> "1", "x5" -> "2.0")

  assert(
    read(prodOrDev from ConfigSource.fromMap(allConfigsExist)) ==
      Right(Left(Prod(Ldap("v1"), DbUrl("v2"))))
  )
}
