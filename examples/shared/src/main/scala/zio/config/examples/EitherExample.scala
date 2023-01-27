package zio.config.examples

import zio.config._

import Config._

object EitherExample extends App {
  final case class Ldap(value: String)  extends AnyVal
  final case class DbUrl(value: String) extends AnyVal

  case class Prod(ldap: Ldap, dburl: DbUrl)
  case class Dev(user: String, password: Int, dburl: Double)

  val prod: Config[Prod] =
    (string("x1").to[Ldap] zip string("x2").to[DbUrl]).to[Prod]

  val dev: Config[Dev] =
    (string("x3") zip int("x4") zip double("x5")).to[Dev]

  val prodOrDev: Config[Either[Prod, Dev]] =
    prod orElseEither dev

  val validProd: Map[String, String] =
    Map("x1" -> "v1", "x2" -> "v2", "x3" -> "v3")

  // Obviously getting a constant map source doesn't need ZIO effect
  val source: ConfigSource           =
    ConfigProvider.fromMap(validProd, "constant")

  val validDev: Map[String, String] =
    Map("x3" -> "v3", "x4" -> "1", "x5" -> "2.0")

  val anotherSource: ConfigSource   =
    ConfigProvider.fromMap(validDev)

  //assert(read(prodOrDev from anotherSource) == Right(Dev("v3", 1, 2.0)))

  val parseErrorConfig: Map[String, String] =
    Map("x2" -> "v2", "x3" -> "v3", "x4" -> "1", "x5" -> "notadouble")

  val invalidSource: ConfigSource           =
    ConfigProvider.fromMap(parseErrorConfig, "constant")

  println(
    read(prodOrDev from invalidSource).mapError(_.prettyPrint()).either.unsafeRun
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
  val allConfigsExist: Map[String, String] =
    Map("x1" -> "v1", "x2" -> "v2", "x3" -> "v3", "x4" -> "1", "x5" -> "2.0")

  assert(
    read(prodOrDev from ConfigProvider.fromMap(allConfigsExist)) equalM
      Left(Prod(Ldap("v1"), DbUrl("v2")))
  )
}
