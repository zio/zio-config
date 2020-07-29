package zio.config.shapeless

import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.test.Assertion._
import zio.test._
import zio.config._

object OverrideDerivationTestEnv extends DeriveConfigDescriptor {
  override def mapClassName(name: String): String = toSnakeCase(name) + "_suffix"
  override def mapFieldName(name: String): String = "prefix_" + toSnakeCase(name)

  val wrapSealedTraitClasses: Boolean = false
  val wrapSealedTraits: Boolean       = false
}

object OverrideDerivationTestWithWrappedSealedTraitName extends DeriveConfigDescriptor {
  override def mapClassName(name: String): String = toSnakeCase(name)
  override def mapFieldName(name: String): String = toSnakeCase(name)

  val wrapSealedTraitClasses: Boolean = true
  val wrapSealedTraits: Boolean       = true
}

object OverrideDerivationTest extends DefaultRunnableSpec {
  val spec = suite("OverrideDerivationTest")(
    test("simple config") {
      import OverrideDerivationTestEnv._

      case class Cfg(fieldName: String)

      val res = write(getDescriptor[Cfg].configDescriptor, Cfg("a"))

      assert(res)(isRight(equalTo(Record(Map("prefix_field_name" -> Leaf("a")))))) &&
      assert(
        res
          .map(ConfigSource.fromPropertyTree(_, "tree", LeafForSequence.Valid))
          .flatMap(v => read(getDescriptor[Cfg].configDescriptor from v))
      )(
        isRight(equalTo(Cfg("a")))
      )
    },
    test("unwrapped sealed hierarchy") {
      import OverrideDerivationTestEnv._

      sealed trait Inner
      case object Obj1Name                     extends Inner
      case object OtherOBJECT                  extends Inner
      case class ClassWithData(data: String)   extends Inner
      case class ClassWithValue(value: String) extends Inner

      case class Outer(list: List[Inner])

      implicit val cInner: Descriptor[Inner] = getDescriptor[Inner]

      val _ = cInner

      val cfg = Outer(List(OtherOBJECT, Obj1Name, ClassWithValue("a"), ClassWithData("b")))

      val res = write(OverrideDerivationTestEnv.getDescriptor[Outer].configDescriptor, cfg)

      val expected = Record(
        Map(
          "prefix_list" -> Sequence(
            List(
              Leaf("other_object_suffix"),
              Leaf("obj1_name_suffix"),
              Record(Map("prefix_value" -> Leaf("a"))),
              Record(Map("prefix_data"  -> Leaf("b")))
            )
          )
        )
      )

      assert(res)(isRight(equalTo(expected))) &&
      assert(
        res
          .map(ConfigSource.fromPropertyTree(_, "tree", LeafForSequence.Valid))
          .flatMap(v => read(getDescriptor[Outer].configDescriptor from v))
      )(
        isRight(equalTo(cfg))
      )
    },
    test("wrapped sealed hierarchy") {
      import OverrideDerivationTestWithWrappedSealedTraitName._

      sealed trait Inner
      case object Obj1Name                     extends Inner
      case object OtherOBJECT                  extends Inner
      case class ClassWithData(data: String)   extends Inner
      case class ClassWithValue(value: String) extends Inner

      case class Outer(list: List[Inner])

      val cfg = Outer(List(OtherOBJECT, Obj1Name, ClassWithValue("a"), ClassWithData("b")))

      implicit val cInner: Descriptor[Inner] = getDescriptor[Inner]

      val _ = cInner

      val res = write(getDescriptor[Outer].configDescriptor, cfg)

      val expected = Record(
        Map(
          "list" -> Sequence(
            List(
              Record(Map("inner" -> Leaf("other_object"))),
              Record(Map("inner" -> Leaf("obj1_name"))),
              Record(Map("inner" -> Record(Map("class_with_value" -> Record(Map("value" -> Leaf("a"))))))),
              Record(Map("inner" -> Record(Map("class_with_data" -> Record(Map("data" -> Leaf("b")))))))
            )
          )
        )
      )

      assert(res)(isRight(equalTo(expected))) &&
      assert(
        res
          .map(ConfigSource.fromPropertyTree(_, "tree", LeafForSequence.Valid))
          .flatMap(v => read(getDescriptor[Outer].configDescriptor from v))
      )(
        isRight(equalTo(cfg))
      )
    }
  )
}
