package zio.config

import zio.{Config, ConfigProvider}, Config._
import zio.test.Assertion._
import zio.test.{Annotations, _}

import RecursiveConfigTestUtils._

object RecursiveConfigTest extends BaseSpec {

  val spec: Spec[Annotations, Any] =
    suite("RecursiveConfigTest")(
      test("read simple") {
        assertZIO(read(SimpleRec.config from SimpleRec.source))(equalTo(SimpleRec.expected))
      },
      test("read simple list") {
        assertZIO(read(SimpleListRec.config from SimpleListRec.source))(equalTo(SimpleListRec.expected))
      },
      test("read simple either") {
        assertZIO(read(SimpleEitherRec.config from SimpleEitherRec.source))(equalTo(SimpleEitherRec.expected))
      },
      test("read simple reversed") {
        assertZIO(read(SimpleRecReversed.config from SimpleRecReversed.source))(
          equalTo(SimpleRecReversed.expected)
        )
      },
      test("read simple reversed multiple") {
        assertZIO(read(SimpleRecMultiple.config from SimpleRecMultiple.source))(
          equalTo(SimpleRecMultiple.expected)
        )
      },
      test("read simple with updated key") {
        assertZIO(
          read(
            SimpleRecMultiple.config.mapKey(_.toUpperCase()) from SimpleRecMultiple.source
              .mapKeys(_.toLowerCase())
          )
        )(
          equalTo(SimpleRecMultiple.expected)
        )
      },
      test("read mutual recursive") {
        assertZIO(read(data from testSource))(equalTo(recursiveValue))
      },
      test("read expression tree") {
        assertZIO(read(expr from exprSource))(equalTo(exprValue))
      },
      test("write simple") {
        assert(write(SimpleRec.config, SimpleRec.expected))(isRight(equalTo(SimpleRec.tree)))
      },
      test("documentation") {
        assert(generateDocs(SimpleRec.config).toTable)(
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
        val docs  = generateDocs(expr)
        val table = docs.toTable
        assert(table)(
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
                          Some(Table.Format.RecursionList),
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
          )
        )
      } @@ TestAspect.exceptScala211
    )
}

object RecursiveConfigTestUtils {

  case class SimpleRec(id: Int, nested: Option[SimpleRec])

  object SimpleRec {
    val config: Config[SimpleRec] =
      (int("id") zip nested("nested")(config).optional).to[SimpleRec]

    val tree: PropertyTree[String, String] = PropertyTree.Record(
      Map(
        "id"     -> PropertyTree.Leaf("1"),
        "nested" -> PropertyTree.Record(
          Map(
            "id" -> PropertyTree.Leaf("2")
          )
        )
      )
    )

    val expected: SimpleRec =
      SimpleRec(1, Some(SimpleRec(2, None)))

    val source: ConfigSource =
      ConfigSource.fromPropertyTree(
        tree,
        "tree"
      )

  }

  case class SimpleListRec(id: Int, nested: List[SimpleListRec])

  object SimpleListRec {
    val config: Config[SimpleListRec] =
      (int("id") zip listOf("nested")(config)).to[SimpleListRec]

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

    val expected: SimpleListRec = SimpleListRec(1, List(SimpleListRec(2, Nil)))

    val source: ConfigSource =
      ConfigSource
        .fromPropertyTree(
          tree.leafNotASequence,
          "tree"
        )

  }
  case class SimpleEitherRec(id: Int, nested: Either[SimpleEitherRec, Int])

  object SimpleEitherRec {
    val config: Config[SimpleEitherRec] =
      (int("id") zip (nested("nested")(config))
        .orElseEither(int("termination"))).to[SimpleEitherRec]

    val tree: PropertyTree[String, String] = PropertyTree.Record(
      Map(
        "id"     -> PropertyTree.Leaf("1"),
        "nested" -> PropertyTree.Record(
          Map(
            "id"     -> PropertyTree.Leaf("2"),
            "nested" -> PropertyTree.Record(
              Map(
                "id"          -> PropertyTree.Leaf("3"),
                "termination" -> PropertyTree.Leaf("1")
              )
            )
          )
        )
      )
    )

    val expected: SimpleEitherRec =
      SimpleEitherRec(1, Left(SimpleEitherRec(2, Left(SimpleEitherRec(3, Right(1))))))

    val source: ConfigSource =
      ConfigSource
        .fromPropertyTree(
          tree.leafNotASequence,
          "tree"
        )
  }

  case class SimpleRecReversed(nested: Option[SimpleRecReversed], id: Int)

  object SimpleRecReversed {
    val config: Config[SimpleRecReversed] =
      (nested("nested")(config).optional zip int("id")).to[SimpleRecReversed]

    val tree: PropertyTree[String, String] = PropertyTree.Record(
      Map(
        "id"     -> PropertyTree.Leaf("1"),
        "nested" -> PropertyTree.Record(
          Map(
            "id" -> PropertyTree.Leaf("2")
          )
        )
      )
    )

    val expected: SimpleRecReversed = SimpleRecReversed(Some(SimpleRecReversed(None, 2)), 1)

    val source: ConfigSource =
      ConfigSource.fromPropertyTree(
        tree,
        "tree"
      )

  }

  case class SimpleRecMultiple(nested: Option[SimpleRecMultiple], id: Int, nested2: Option[SimpleRecMultiple])

  object SimpleRecMultiple {
    val config: Config[SimpleRecMultiple] =
      (nested("nested")(config).optional zip int("id") zip nested("nested2")(config).optional).to[SimpleRecMultiple]

    val tree: PropertyTree[String, String] = PropertyTree.Record(
      Map(
        "id"     -> PropertyTree.Leaf("1"),
        "nested" -> PropertyTree.Record(
          Map(
            "id" -> PropertyTree.Leaf("2")
          )
        )
      )
    )

    val source: ConfigSource =
      ConfigSource.fromPropertyTree(
        tree,
        "tree"
      )

    val expected: SimpleRecMultiple = SimpleRecMultiple(Some(SimpleRecMultiple(None, 2, None)), 1, None)
  }

  case class Data(rows: Row)

  case class Row(id: Int, nested: Option[Data])

  lazy val row: Config[Row]   = (int("id") zip data.optional).to[Row]
  lazy val data: Config[Data] = nested("rows")(row).to[Data]

  val testSource: ConfigSource = ConfigSource.fromPropertyTree(
    PropertyTree.Record(
      Map(
        "rows" -> PropertyTree.Record(
          Map(
            "id"   -> PropertyTree.Leaf("1"),
            "rows" -> PropertyTree.Record(
              Map(
                "id" -> PropertyTree.Leaf("2")
              )
            )
          )
        )
      )
    ),
    "tree"
  )

  val recursiveValue: Data = Data(Row(1, Some(Data(Row(2, None)))))

  sealed trait Expr
  case class Lit(n: Int)            extends Expr
  case class Add(items: List[Expr]) extends Expr

  lazy val expr: Config[Expr] = {
    val lit: Config[Expr] = int.transformOrFail(
      n => Right(Lit(n)),
      {
        case Lit(n) => Right(n)
        case _      => Left(s"Not Lit")
      }
    )

    val add: Config[Expr] = nested("add")(
      listOf(expr)
        .transformOrFailRight(
          lst => Add(lst),
          {
            case Add(items) => Right(items)
            case _          => Left("Failed to write")
          }
        )
    )

    lit <> add
  }

  val exprValue: Add           = Add(List(Lit(1), Add(List(Add(List(Lit(2), Lit(3))), Lit(4))), Lit(5)))
  val exprSource: ConfigSource = ConfigSource.fromPropertyTree(
    write(expr, exprValue) match {
      case Left(_)      => ???
      case Right(value) => value
    },
    "test"
  )
}
