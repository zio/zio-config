package zio.config.examples

import zio.config.ConfigSource
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.PropertyTree.Leaf
import zio.config.PropertyTree
import zio.config.singleton
import zio.config.ConfigDescriptor._
import zio.config.typesafe._

// List works quite nicely if the source is typesafe HOCON. Refer typesafe examples
object ListExample extends App with EitherImpureOps {
  final case class PgmConfig(a: String, b: List[String])

  val multiMap =
    Map(
      "xyz"     -> singleton("something"),
      "regions" -> ::("australia", List("canada", "usa"))
    )

  val config: ConfigDescriptor[PgmConfig] =
    (string("xyz") |@| list("regions")(string))(PgmConfig.apply, PgmConfig.unapply)

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

  val propertyTree =
    write(config, PgmConfig("something", List("australia", "canada", "usa")))

  assert(
    propertyTree ==
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

  println(propertyTree.map(_.toHocon))
  // Right(SimpleConfigObject({"regions":["australia","canada","usa"],"xyz":"something"}))
  println(propertyTree.map(_.toHocon.render()))

//  Right({
//    # hardcoded value
//      "regions" : [
//    # hardcoded value
//      "australia",
//    # hardcoded value
//      "canada",
//    # hardcoded value
//      "usa"
//    ],
//    # hardcoded value
//      "xyz" : "something"
//  }
// )

}
