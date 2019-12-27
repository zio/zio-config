package zio.config.examples

import zio.config._, ConfigDescriptor._
import zio.DefaultRuntime
import zio.config.PropertyTree, PropertyTree.{ Leaf, Record }

object ListExample extends App {
  final case class PgmConfig(a: String, b: List[String])

  val multiMap =
    Map(
      "xyz"         -> singleton("something"),
      "aws.regions" -> ::("australia", List("canada", "usa"))
    )

  val conf: ConfigDescriptor[String, String, PgmConfig] =
    (string("xyz") |@| list("aws")(string("regions")))(PgmConfig.apply, PgmConfig.unapply)

  val runtime = new DefaultRuntime {}

  val resultFromMultiMap =
    runtime.unsafeRun(
      Config.fromMultiMap(multiMap, conf).flatMap(_.config.config)
    )

  assert(
    resultFromMultiMap ==
      PgmConfig("something", List("australia", "canada", "usa"))
  )

  assert(
    write(conf, resultFromMultiMap) ==
      Right(
        PropertyTree.Record(
          Map(
            "aws" -> PropertyTree.Sequence(
              List(
                Record(Map("regions" -> Leaf("australia"))),
                Record(Map("regions" -> Leaf("canada"))),
                Record(Map("regions" -> Leaf("usa")))
              )
            ),
            "xyz" -> Leaf("something")
          )
        )
      )
  )

  // Note that, this may also give you some result, but this isn't actually valid
  // because, in this case, we expect a key value pair (a conf) in outer base path, which cannot be represented using a simple map(string, ::(string))
  // val anotherComplexConf =
  // val conf = list("outer")(conf)
  //
  // val multiMapComplex =
  // Map(
  //  "outer.xyz"         -> singleton("something"),
  //  "outer.aws.regions" -> ::("australia", List("canada", "usa"))
  // )
  //  val result =
  // runtime.unsafeRun(Config.fromMultiMap(multiMapComplex, anotherComplexConf).flatMap(_.config.config))
  // In short, in use cases of list("outer")(conf), a valid source can be only a json/hoccon
  // Using type safe config
}
