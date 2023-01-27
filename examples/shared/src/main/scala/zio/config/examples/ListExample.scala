package zio.config.examples

import zio.ZIO
import zio.{Config, ConfigProvider}, Config._
import zio.config.PropertyTree.Leaf
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.typesafe._
import zio.config.{PropertyTree, _}

// List works quite nicely if the source is typesafe HOCON. Refer typesafe examples
object ListExample extends App with EitherImpureOps {
  final case class PgmConfig(a: String, b: List[String])

  // Fails if regions had only one element,
  val map: Map[String, String] =
    Map(
      "xyz"     -> "something",
      "regions" -> "australia, canada, usa"
    )

  val config: Config[PgmConfig] =
    (string("xyz") zip listOf("regions")(string)).to[PgmConfig]

  val mapSource: ConfigSource =
    ConfigProvider.fromMap(map, "constant", keyDelimiter = None, valueDelimiter = Some(','))

  val resultFromMultiMap: ZIO[Any, Config.Error, PgmConfig] =
    read(config from mapSource)

  val expected: PgmConfig =
    PgmConfig("something", List("australia", "canada", "usa"))

  assert(
    resultFromMultiMap equalM
      PgmConfig("something", List("australia", "canada", "usa"))
  )

  val propertyTree: Either[String, PropertyTree[String, String]] =
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
