package zio.config.shapeless

import zio.config._
import ConfigDescriptorAdt._
import zio.config.PropertyTree
import zio.config.PropertyTree.Leaf
import zio.config.PropertyTree.Record
import zio.test.Assertion._
import zio.test._

object DerivationTest extends DefaultRunnableSpec {
  val spec = suite("DerivationTest")(
    test("support describe annotation") {
      @describe("class desc")
      case class Cfg(@describe("field desc") fname: String)

      def collectDescriptions[T](
        desc: ConfigDescriptor[T],
        path: Option[String]
      ): List[(Option[String], String)] = desc match {
        case Default(config, _)        => collectDescriptions(config.value, path)
        case DynamicMap(_, config)     => collectDescriptions(config.value, path)
        case Describe(config, message) => (path, message) :: collectDescriptions(config.value, path)
        case Nested(_, path, config)   => collectDescriptions(config.value, Some(path))
        case Optional(config)          => collectDescriptions(config.value, path)
        case OrElse(left, right) =>
          collectDescriptions(left.value, path) ::: collectDescriptions(right.value, path)
        case OrElseEither(left, right) =>
          collectDescriptions(left.value, path) ::: collectDescriptions(right.value, path)
        case Sequence(_, config) => collectDescriptions(config.value, path)
        case Source(_, _)        => Nil
        case Zip(left, right) =>
          collectDescriptions(left.value, path) ::: collectDescriptions(right.value, path)
        case XmapEither(config, _, _) => collectDescriptions(config.value, path)
      }

      assert(collectDescriptions(DeriveConfigDescriptor.descriptor[Cfg], None))(
        contains((None: Option[String]) -> "class desc") &&
          contains(None                 -> "field desc")
      )
    },
    test("support name annotation") {
      val customDerivation = new DeriveConfigDescriptor {
        override def mapClassName(name: String): String = name
        override def mapFieldName(name: String): String = name
        override def wrapSealedTraitClasses: Boolean    = true
        override def wrapSealedTraits: Boolean          = true
      }

      @name("SealedTrait")
      sealed trait St
      @name("className")
      case class Cfg(@name("otherName") fname: String) extends St

      def collectPath[T](desc: ConfigDescriptor[T]): List[String] = desc match {
        case Default(config, _)        => collectPath(config.value)
        case Describe(config, _)       => collectPath(config.value)
        case DynamicMap(_, config)     => collectPath(config.value)
        case Nested(_, path, config)   => path :: collectPath(config.value)
        case Optional(config)          => collectPath(config.value)
        case OrElse(left, right)       => collectPath(left.value) ::: collectPath(right.value)
        case OrElseEither(left, right) => collectPath(left.value) ::: collectPath(right.value)
        case Sequence(_, config)       => collectPath(config.value)
        case Source(_, _)              => Nil
        case Zip(left, right)          => collectPath(left.value) ::: collectPath(right.value)
        case XmapEither(config, _, _)  => collectPath(config.value)
      }

      // IntelliJ will hide this, however it is required
      import customDerivation._

      assert(collectPath(customDerivation.getDescriptor[St].configDescriptor))(
        equalTo("SealedTrait" :: "className" :: "otherName" :: Nil)
      )
    },
    test("support default value") {
      @describe("class desc")
      case class Cfg(fname: String = "defaultV")

      def collectDefault[T](
        desc: ConfigDescriptor[T],
        path: Option[String]
      ): List[(Option[String], Any)] = desc match {
        case Default(config, v)      => (path -> v) :: collectDefault(config.value, path)
        case Describe(config, _)     => collectDefault(config.value, path)
        case DynamicMap(_, config)   => collectDefault(config.value, path)
        case Nested(_, path, config) => collectDefault(config.value, Some(path))
        case Optional(config)        => collectDefault(config.value, path)
        case OrElse(left, right)     => collectDefault(left.value, path) ::: collectDefault(right.value, path)
        case OrElseEither(left, right) =>
          collectDefault(left.value, path) ::: collectDefault(right.value, path)
        case Sequence(_, config)      => collectDefault(config.value, path)
        case Source(_, _)             => Nil
        case Zip(left, right)         => collectDefault(left.value, path) ::: collectDefault(right.value, path)
        case XmapEither(config, _, _) => collectDefault(config.value, path)
      }

      assert(collectDefault(DeriveConfigDescriptor.descriptor[Cfg], None))(equalTo((None, "defaultV") :: Nil))
    },
    test("support lists recursive") {
      case class A1(a: List[String])
      case class A2(a: List[A1])
      case class A3(a: List[A2])
      case class A4(a: List[A3])
      case class A5(a: List[A4])

      def loop(depth: Int): PropertyTree[String, String] =
        if (depth > 0) Record(Map("a" -> PropertyTree.Sequence(List(loop(depth - 1)))))
        else Leaf("str")

      val src = ConfigSource.fromPropertyTree(loop(5), "tree", LeafForSequence.Valid)

      val res = read(DeriveConfigDescriptor.descriptor[A5] from src)

      assert(res)(isRight(anything))
    },
    test("support lists non-recursive") {
      import NonRecursiveListHelper._

      def loop(depth: Int): PropertyTree[String, String] =
        if (depth > 0) Record(Map("a" -> PropertyTree.Sequence(List(loop(depth - 1)))))
        else Leaf("str")

      val src = ConfigSource.fromPropertyTree(loop(5), "tree", LeafForSequence.Valid)

      val res = read(NonRecursiveDerivation.getDescriptor[A5].configDescriptor from src)

      assert(res)(isRight(anything))
    },
    test("support nested lists non-recursive") {
      import NonRecursiveDerivation.{ getDescriptor, Descriptor }

      case class A(a: List[String])
      implicit val cA: Descriptor[A] = getDescriptor[A]
      val _                          = cA
      case class B(a: List[List[List[List[List[List[List[List[List[List[A]]]]]]]]]])

      def loop(depth: Int): PropertyTree[String, String] =
        if (depth > 0) PropertyTree.Sequence(List(loop(depth - 1)))
        else Record(Map("a" -> PropertyTree.Sequence(List(Leaf("s")))))

      val src = ConfigSource.fromPropertyTree(Record(Map("a" -> loop(10))), "tree", LeafForSequence.Valid)

      val res = read(getDescriptor[B].configDescriptor from src)

      assert(res)(isRight(anything))
    },
    test("support nested lists recursive") {
      case class A(a: List[String])
      case class B(a: List[List[List[List[List[List[List[List[List[List[A]]]]]]]]]])

      def loop(depth: Int): PropertyTree[String, String] =
        if (depth > 0) PropertyTree.Sequence(List(loop(depth - 1)))
        else Record(Map("a" -> PropertyTree.Sequence(List(Leaf("s")))))

      val src = ConfigSource.fromPropertyTree(Record(Map("a" -> loop(10))), "tree", LeafForSequence.Valid)

      val res = read(DeriveConfigDescriptor.descriptor[B] from src)

      assert(res)(isRight(anything))
    }
  )
}

object NonRecursiveListHelper {
  import NonRecursiveDerivation._

  case class A1(a: List[String])
  implicit val cA1: Descriptor[A1] = getDescriptor[A1]
  case class A2(a: List[A1])
  implicit val cA2: Descriptor[A2] = getDescriptor[A2]
  case class A3(a: List[A2])
  implicit val cA3: Descriptor[A3] = getDescriptor[A3]
  case class A4(a: List[A3])
  implicit val cA4: Descriptor[A4] = getDescriptor[A4]
  case class A5(a: List[A4])

}
