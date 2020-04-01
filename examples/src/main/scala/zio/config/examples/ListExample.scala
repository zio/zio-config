package zio.config.examples

import zio.config.ConfigDescriptor._
import zio.config._
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.PropertyTree.Leaf

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

  assert(
    write(config, PgmConfig("something", List("australia", "canada", "usa"))) ==
      Right(
        PropertyTree
          .Record(
            Map(
              "xyz"     -> Leaf("something"),
              "regions" -> PropertyTree.Sequence(List(Leaf("australia"), Leaf("canada"), Leaf("usa")))
            )
          )
      )
  )
}
