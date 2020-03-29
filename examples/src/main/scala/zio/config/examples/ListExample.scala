package zio.config.examples

import zio.config.ConfigDescriptor._
//import zio.config.PropertyTree.{ Leaf, Record }
import zio.config._
import zio.config.examples.typesafe.EitherImpureOps

// List works quite nicely if the source is typesafe HOCON. Refer typesafe examples
object ListExample extends App with EitherImpureOps {
  final case class PgmConfig(a: String, b: List[String])

  val multiMap =
    Map(
      "xyz"     -> singleton("something"),
      "regions" -> ::("australia", List("canada", "usa"))
    )

  val config: ConfigDescriptor[String, String, PgmConfig] =
    (string("xyz") |@| list(string("regions")))(PgmConfig.apply, PgmConfig.unapply)

  val runtime = zio.Runtime.default

  val tree =
    ConfigSource.fromMultiMap(multiMap, "constant")

  val resultFromMultiMap =
    read(config from ConfigSource.fromMultiMap(multiMap, "constant"))

  val expected =
    PgmConfig("something", List("australia", "canada", "usa"))

  assert(
    resultFromMultiMap ==
      Right(
        PgmConfig("something", List("australia", "canada", "usa"))
      )
  )
}
