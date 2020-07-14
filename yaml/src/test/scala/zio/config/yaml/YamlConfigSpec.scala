package zio.config.yaml

import zio.test._
import zio.test.Assertion._
import zio.config.PropertyTree

object YamlConfigSpec extends DefaultRunnableSpec {
  val spec = suite("YamlConfig")(
    testM("Read a complex structure") {
      val result = YamlConfigSource.fromString(
        """
          |top:
          |  child:
          |    i: 1
          |    b: true
          |    s: "str"
          |  list:
          |  - i: 1
          |  - b: true
          |  - s: "str"
          |""".stripMargin
      )
      val expected =
        PropertyTree.Record(
          Map(
            "top" -> PropertyTree.Record(
              Map(
                "child" -> PropertyTree.Record(
                  Map(
                    "i" -> PropertyTree.Leaf("1"),
                    "b" -> PropertyTree.Leaf("true"),
                    "s" -> PropertyTree.Leaf("str")
                  )
                ),
                "list" -> PropertyTree.Sequence(
                  List(
                    PropertyTree.Record(Map("i" -> PropertyTree.Leaf("1"))),
                    PropertyTree.Record(Map("b" -> PropertyTree.Leaf("true"))),
                    PropertyTree.Record(Map("s" -> PropertyTree.Leaf("str")))
                  )
                )
              )
            )
          )
        )

      result
        .map(_.getConfigValue(List()))
        .run
        .map(assert(_)(succeeds(equalTo(expected))))
    }
  )
}
