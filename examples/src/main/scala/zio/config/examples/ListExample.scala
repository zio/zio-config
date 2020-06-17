package zio.config.examples

import zio.config.ConfigSource
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.PropertyTree.Leaf
import zio.config.PropertyTree
import zio.config.ConfigDescriptor._
import zio.config._, zio.config.typesafe._

// List works quite nicely if the source is typesafe HOCON. Refer typesafe examples
object ListExample extends App with EitherImpureOps {
  final case class PgmConfig(a: String, b: List[String])

  val map =
    Map(
      "xyz"     -> "something",
      "regions" -> "australia, canada, usa"
    )

  val config: ConfigDescriptor[PgmConfig] =
    (string("xyz") |@| list("regions")(string))(PgmConfig.apply, PgmConfig.unapply)

  val mapSource =
    ConfigSource.fromMap(map, "constant", keyDelimiter = None, valueDelimiter = Some(','))

  val resultFromMultiMap =
    read(config from mapSource)

  println(resultFromMultiMap)
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
