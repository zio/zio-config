package zio.config

import zio.test._
import zio.test.Assertion._

import ConfigDescriptor._
import RecursiveConfigTestUtils._

object RecursiveConfigTest
    extends BaseSpec(
      suite("RecursiveConfigTest")(
        test("read simple") {
          assert(read(simpleRec from simpleTestSource))(isRight(equalTo(simpleRecursiveValue)))
        },
        test("read mutual recursive") {
          assert(read(data from testSource))(isRight(equalTo(recursiveValue)))
        }
      )
    )

object RecursiveConfigTestUtils {

  case class SimpleRec(id: Int, nested: Option[SimpleRec])

  lazy val simpleRec: ConfigDescriptor[SimpleRec] =
    (int("id") |@| nested("nested")(simpleRec).optional)(SimpleRec.apply, SimpleRec.unapply)

  val simpleTestSource: ConfigSource = ConfigSource.fromPropertyTree(
    PropertyTree.Record(
      Map(
        "id" -> PropertyTree.Leaf("1"),
        "nested" -> PropertyTree.Record(
          Map(
            "id" -> PropertyTree.Leaf("2")
          )
        )
      )
    ),
    "tree",
    LeafForSequence.Valid
  )

  val simpleRecursiveValue: SimpleRec = SimpleRec(1, Some(SimpleRec(2, None)))

  case class Data(rows: Row)

  case class Row(id: Int, nested: Option[Data])

  lazy val row: ConfigDescriptor[Row]   = (int("id") |@| data.optional)(Row.apply, Row.unapply)
  lazy val data: ConfigDescriptor[Data] = nested("rows")(row)(Data.apply, Data.unapply)

  val testSource: ConfigSource = ConfigSource.fromPropertyTree(
    PropertyTree.Record(
      Map(
        "rows" -> PropertyTree.Record(
          Map(
            "id" -> PropertyTree.Leaf("1"),
            "rows" -> PropertyTree.Record(
              Map(
                "id" -> PropertyTree.Leaf("2")
              )
            )
          )
        )
      )
    ),
    "tree",
    LeafForSequence.Valid
  )

  val recursiveValue: Data = Data(Row(1, Some(Data(Row(2, None)))))

}
