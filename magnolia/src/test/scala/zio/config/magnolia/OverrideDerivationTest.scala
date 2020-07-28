package zio.config.magnolia

import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.test.Assertion._
import zio.test._
import zio.config._

object OverrideDerivationTestEnv extends DeriveConfigDescriptor {
  import Descriptor.SealedTraitStrategy._

  override def mapClassName(name: String): String = toSnakeCase(name) + "_suffix"
  override def mapFieldName(name: String): String = "prefix_" + toSnakeCase(name)

  override def sealedTraitStrategy: Descriptor.SealedTraitStrategy =
    ignoreSealedTraitName && ignoreSubClassName
}

object OverrideDerivationTest extends DefaultRunnableSpec {
  val spec = suite("OverrideDerivationTest")(
    test("simple config") {
      import OverrideDerivationTestEnv._

      case class Cfg(fieldName: String)

      val res = write(getDescriptor[Cfg].desc, Cfg("a"))

      assert(res)(isRight(equalTo(Record(Map("prefix_field_name" -> Leaf("a")))))) &&
      assert(
        res
          .map(ConfigSource.fromPropertyTree(_, "tree", LeafForSequence.Valid))
          .flatMap(v => read(getDescriptor[Cfg].desc from v))
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

      val cfg = Outer(List(OtherOBJECT, Obj1Name, ClassWithValue("a"), ClassWithData("b")))

      val res = write(OverrideDerivationTestEnv.getDescriptor[Outer].desc, cfg)

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
          .flatMap(v => read(getDescriptor[Outer].desc from v))
      )(
        isRight(equalTo(cfg))
      )
    },
    test("wrapped sealed hierarchy") {
      val wrappedSealedHierarchy = new DeriveConfigDescriptor {
        import Descriptor.SealedTraitStrategy._

        override def sealedTraitStrategy: Descriptor.SealedTraitStrategy =
          wrapSealedTraitName && wrapSubClassName
      }

      import wrappedSealedHierarchy._

      sealed trait Inner
      case object Obj1Name                     extends Inner
      case object OtherOBJECT                  extends Inner
      case class ClassWithData(data: String)   extends Inner
      case class ClassWithValue(value: String) extends Inner

      case class Outer(list: List[Inner])

      val cfg = Outer(List(OtherOBJECT, Obj1Name, ClassWithValue("a"), ClassWithData("b")))

      val res = write(getDescriptor[Outer].desc.mapKey(zio.config.toSnakeCase), cfg)

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
          .flatMap(v => read(wrappedSealedHierarchy.getDescriptor[Outer].desc.mapKey(zio.config.toSnakeCase) from v))
      )(
        isRight(equalTo(cfg))
      )
    },
    test("config with type labels ignoring the name of sealed trait") {
      val wrappedSealedHierarchy = new DeriveConfigDescriptor {
        import Descriptor.SealedTraitStrategy._

        override def sealedTraitStrategy: Descriptor.SealedTraitStrategy =
          ignoreSealedTraitName && labelSubClassName("type")
      }

      import wrappedSealedHierarchy._

      sealed trait Inner
      case object Obj1Name                     extends Inner
      case object OtherOBJECT                  extends Inner
      case class ClassWithData(data: String)   extends Inner
      case class ClassWithValue(value: String) extends Inner

      case class Outer(list: List[Inner])

      val cfg = Outer(List(OtherOBJECT, Obj1Name, ClassWithValue("a"), ClassWithData("b")))

      val res = write(getDescriptor[Outer].desc.mapKey(zio.config.toSnakeCase), cfg)

      val expected = Record(
        Map(
          "list" -> Sequence(
            List(
              Leaf("other_object"),
              Leaf("obj1_name"),
              Record(Map("value" -> Leaf("a"), "type" -> Leaf("class_with_value"))),
              Record(Map("data"  -> Leaf("b"), "type" -> Leaf("class_with_data")))
            )
          )
        )
      )
      assert(res)(isRight(equalTo(expected))) &&
      assert(
        res
          .map(ConfigSource.fromPropertyTree(_, "tree", LeafForSequence.Valid))
          .flatMap(v => read(wrappedSealedHierarchy.getDescriptor[Outer].desc.mapKey(zio.config.toSnakeCase) from v))
      )(
        isRight(equalTo(cfg))
      )
    },
    test("config with type labels wrapped with sealed trait name") {
      val wrappedSealedHierarchy = new DeriveConfigDescriptor {
        import Descriptor.SealedTraitStrategy._

        override def sealedTraitStrategy: Descriptor.SealedTraitStrategy =
          wrapSealedTraitName && labelSubClassName("type")
      }

      import wrappedSealedHierarchy._

      sealed trait Inner
      case object Obj1Name                     extends Inner
      case object OtherOBJECT                  extends Inner
      case class ClassWithData(data: String)   extends Inner
      case class ClassWithValue(value: String) extends Inner

      case class Outer(list: List[Inner])

      val cfg = Outer(List(OtherOBJECT, Obj1Name, ClassWithValue("a"), ClassWithData("b")))

      val res = write(getDescriptor[Outer].desc.mapKey(zio.config.toSnakeCase), cfg)

      val expected = Record(
        Map(
          "list" -> Sequence(
            List(
              Record(Map("inner" -> Leaf("other_object"))),
              Record(Map("inner" -> Leaf("obj1_name"))),
              Record(
                Map(
                  "inner" -> Record(Map("value" -> Leaf("a"), "type" -> Leaf("class_with_value")))
                )
              ),
              Record(
                Map(
                  "inner" -> Record(Map("data" -> Leaf("b"), "type" -> Leaf("class_with_data")))
                )
              )
            )
          )
        )
      )
      assert(res)(isRight(equalTo(expected))) &&
      assert(
        res
          .map(ConfigSource.fromPropertyTree(_, "tree", LeafForSequence.Valid))
          .flatMap(v => read(wrappedSealedHierarchy.getDescriptor[Outer].desc.mapKey(zio.config.toSnakeCase) from v))
      )(
        isRight(equalTo(cfg))
      )
    }
  )
}
