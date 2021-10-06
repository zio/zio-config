package zio.config

import zio.config.ConfigDescriptor._

object Example extends App {
  case class SimpleListRec(id: Int, nested: List[SimpleListRec])

  val config_ : ConfigDescriptor[SimpleListRec] =
    (int("id") |@| list("nested")(config_)).to[SimpleListRec]

  val tree: PropertyTree[String, String] =
    PropertyTree.Record(
      Map(
        "id"     -> PropertyTree.Leaf("1"),
        "nested" -> PropertyTree.Sequence(
          List(
            PropertyTree.Record(
              Map(
                "id"     -> PropertyTree.Leaf("2"),
                "nested" -> PropertyTree.Sequence(Nil)
              )
            )
          )
        )
      )
    )

  zio.Runtime.default.unsafeRun(read(config_ from ConfigSource.fromPropertyTree(tree, "tree")))

}
