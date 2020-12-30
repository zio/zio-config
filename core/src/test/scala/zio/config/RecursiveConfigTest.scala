//FIXME Make Recursion work without losing the view of errors

package zio.config

import zio.test._
import zio.test.Assertion._
import RecursiveConfigTestUtils._
import zio.config.ConfigDescriptor._, ConfigDescriptorAdt._

object RecursiveConfigTest
    extends BaseSpec(
      suite("RecursiveConfigTest")(
        test("read simple") {
          // FIXME:The logic works, more work required in cleaning up the logic.
          assert(read(simpleRec from simpleTestSource))(isRight(equalTo(simpleRecursiveValue)))
        },
        test("read simple reversed") {
          // FIXME:The logic works, more work required in cleaning up the logic.
          assert(read(simpleRecReversed from simpleTestSource))(isRight(equalTo(simpleRecursiveReversedValue)))
        },
        test("read simple reversed multiple") {
          // FIXME:The logic works, more work required in cleaning up the logic.
          assert(read(simpleRecMultiple from simpleTestSource))(isRight(equalTo(simpleRecursiveMultiple)))
        },
        test("read mutual recursive") {
          assert(read(data from testSource))(isRight(equalTo(recursiveValue)))
        },
        test("read expression tree") {
          assert(read(expr from exprSource))(isRight(equalTo(exprValue)))
        },
        test("write simple") {
          assert(write(simpleRec, simpleRecursiveValue))(isRight(equalTo(simpleTestTree)))
        },
        test("documentation") {
          assert(generateDocs(simpleRec).toTable)(
            equalTo(
              Table(
                List(
                  Table.TableRow(
                    Nil,
                    Some(Table.Format.AllOf),
                    Nil,
                    Some(
                      Table(
                        List(
                          Table.TableRow(
                            List(Table.FieldName.Key("id")),
                            Some(Table.Format.Primitive),
                            List(ConfigDocs.Description(Some("id"), "value of type int")),
                            None,
                            Set()
                          ),
                          Table.TableRow(
                            List(Table.FieldName.Key("nested")),
                            Some(Table.Format.Recursion),
                            List(ConfigDocs.Description(None, "optional value")),
                            None,
                            Set()
                          )
                        )
                      )
                    ),
                    Set()
                  )
                )
              )
            )
          )
        },
        test("documentation of expression tree") {
          assert(generateDocs(expr).toTable)(
            equalTo(
              Table(
                List(
                  Table.TableRow(
                    List(),
                    Some(Table.Format.AnyOneOf),
                    List(),
                    Some(
                      Table(
                        List(
                          Table.TableRow(
                            List(),
                            Some(Table.Format.Primitive),
                            List(ConfigDocs.Description(None, "value of type int")),
                            None,
                            Set()
                          ),
                          Table.TableRow(
                            List(Table.FieldName.Key("add")),
                            Some(Table.Format.List),
                            List(),
                            Some(
                              Table(
                                List(
                                  Table.TableRow(
                                    List(),
                                    Some(Table.Format.Primitive),
                                    List(ConfigDocs.Description(None, "value of type int")),
                                    None,
                                    Set()
                                  ),
                                  Table.TableRow(
                                    List(Table.FieldName.Key("add")),
                                    Some(Table.Format.Recursion),
                                    List(),
                                    None,
                                    Set()
                                  )
                                )
                              )
                            ),
                            Set()
                          )
                        )
                      )
                    ),
                    Set()
                  )
                )
              )
            )
          )
        } @@ TestAspect.exceptScala211
      )
    )

object RecursiveConfigTestUtils {

  case class SimpleRec(id: Int, nested: Option[SimpleRec])

  val simpleRec: ConfigDescriptor[SimpleRec] =
    (int("id") |@| nested("nested")(simpleRec).optional)(SimpleRec.apply, SimpleRec.unapply)

  val simpleTestTree: PropertyTree[String, String] = PropertyTree.Record(
    Map(
      "id" -> PropertyTree.Leaf("1"),
      "nested" -> PropertyTree.Record(
        Map(
          "id" -> PropertyTree.Leaf("2")
        )
      )
    )
  )

  val simpleTestSource: ConfigSource = ConfigSource.fromPropertyTree(
    simpleTestTree,
    "tree",
    LeafForSequence.Valid
  )

  case class SimpleRecReversed(nested: Option[SimpleRecReversed], id: Int)

  val simpleRecReversed: ConfigDescriptor[SimpleRecReversed] =
    (nested("nested")(simpleRecReversed).optional |@| int("id"))(SimpleRecReversed.apply, SimpleRecReversed.unapply)

  case class SimpleRecMultiple(nested: Option[SimpleRecMultiple], id: Int, nested2: Option[SimpleRecMultiple])

  val simpleRecMultiple: ConfigDescriptor[SimpleRecMultiple] =
    (nested("nested")(simpleRecMultiple).optional |@| int("id") |@| nested("nested2")(simpleRecMultiple).optional)(
      SimpleRecMultiple.apply,
      SimpleRecMultiple.unapply
    )

  val simpleRecursiveValue: SimpleRec                 = SimpleRec(1, Some(SimpleRec(2, None)))
  val simpleRecursiveReversedValue: SimpleRecReversed = SimpleRecReversed(Some(SimpleRecReversed(None, 2)), 1)
  val simpleRecursiveMultiple: SimpleRecMultiple      = SimpleRecMultiple(Some(SimpleRecMultiple(None, 2, None)), 1, None)

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

  sealed trait Expr
  case class Lit(n: Int)            extends Expr
  case class Add(items: List[Expr]) extends Expr

  def expr: ConfigDescriptor[Expr] = {
    val lit: ConfigDescriptor[Expr] = int.transformOrFail(
      n => Right(Lit(n)), {
        case Lit(n) => Right(n)
        case _      => Left(s"Not Lit")
      }
    )

    val add: ConfigDescriptor[Expr] = nested("add")(
      list(expr)
        .apply(
          lst => Add(lst), {
            case Add(items) => Some(items)
            case _          => None
          }
        )
    )

    lit <> add
  }

  val exprValue = Add(List(Lit(1), Add(List(Add(List(Lit(2), Lit(3))), Lit(4))), Lit(5)))
  val exprSource = ConfigSource.fromPropertyTree(write(expr, exprValue) match {
    case Left(_)      => ???
    case Right(value) => value
  }, "test", LeafForSequence.Invalid)
}
