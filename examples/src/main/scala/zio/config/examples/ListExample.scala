package zio.config.examples

import zio.config.examples.typesafe.EitherImpureOps
import zio.config.PropertyTree.Leaf
import zio.config.PropertyTree
import zio.config.ConfigDescriptor._
import zio.config._, zio.config.typesafe._

// List works quite nicely if the source is typesafe HOCON. Refer typesafe examples
object ListExample extends App with EitherImpureOps {
  final case class PgmConfig(a: String, b: List[String])

  // Fails if regions had only one element,
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

  println(propertyTree.map(_.flattenString()))
  // Right(Map(xyz -> List(something), regions -> List(australia, canada, usa)))

  println(propertyTree.map(_.toJson))

  /*
  Right({
    "regions" : [
       "australia",
       "canada",
       "usa"
    ],
    "xyz" : "something"
  })
 */

}
