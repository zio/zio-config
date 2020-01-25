package zio.config.examples

import zio.config._, ConfigDescriptor._
import zio.DefaultRuntime
import zio.config.PropertyTree, PropertyTree.{ Leaf, Record }

// List works quite nicely if the source is typesafe HOCON. Refer TypesafeConfigHoconExample.scala to get an idea.
object ListExample extends App {
  final case class PgmConfig(b: List[String])

  val multiMap =
    Map(
      "xyz"     -> singleton("something"),
      "regions" -> ::("australia", List("canada", "usa"))
    )

  val config: ConfigDescriptor[String, String, PgmConfig] =
    list(string("regions")).xmap(PgmConfig.apply)(_.b)

  val runtime = new DefaultRuntime {}

  val resultFromMultiMap =
    runtime.unsafeRun(
      read(config from ConfigSource.fromMultiMap(multiMap))
    )

  assert(
    resultFromMultiMap ==
      PgmConfig(List("australia", "canada", "usa"))
  )

  assert(
    write(config, resultFromMultiMap) ==
      Right(
        PropertyTree.Sequence(
          List(
            Record(
              Map("xyz" -> Leaf("something"))
            ),
            Record(Map("regions" -> Leaf("australia"))),
            Record(Map("regions" -> Leaf("canada"))),
            Record(Map("regions" -> Leaf("usa")))
          )
        )
      )
  )

  // Keep a note that, handling list in a flattened map like structure may not be what you need to do, have a look at TypesafeConfigHoconExample.
}
