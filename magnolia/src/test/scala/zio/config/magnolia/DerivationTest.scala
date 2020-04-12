package zio.config.magnolia

import zio.config.ConfigDescriptor.{ Describe, Nested }
import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.test.Assertion._
import zio.test._

object DerivationTest extends DefaultRunnableSpec {
  val spec = suite("DerivationTest")(
    test("support describe annotation") {
      @describe("class desc")
      case class Cfg(@describe("field desc") fname: String)

      def collectDescriptions[T](
        desc: ConfigDescriptor[String, String, T],
        path: Option[String]
      ): List[(Option[String], String)] = desc match {
        case ConfigDescriptor.Default(config, _)    => collectDescriptions(config, path)
        case ConfigDescriptor.DynamicMap(_, config) => collectDescriptions(config, path)
        case Describe(config, message)              => (path, message) :: collectDescriptions(config, path)
        case Nested(path, config)                   => collectDescriptions(config, Some(path))
        case ConfigDescriptor.Optional(config)      => collectDescriptions(config, path)
        case ConfigDescriptor.OrElse(left, right) =>
          collectDescriptions(left, path) ::: collectDescriptions(right, path)
        case ConfigDescriptor.OrElseEither(left, right) =>
          collectDescriptions(left, path) ::: collectDescriptions(right, path)
        case ConfigDescriptor.Sequence(_, config) => collectDescriptions(config, path)
        case ConfigDescriptor.Source(_, _)        => Nil
        case ConfigDescriptor.Zip(left, right) =>
          collectDescriptions(left, path) ::: collectDescriptions(right, path)
        case ConfigDescriptor.XmapEither(config, _, _) => collectDescriptions(config, path)
      }

      assert(collectDescriptions(descriptor[Cfg], None))(
        contains((None: Option[String]) -> "class desc") &&
          contains(Some("fname")        -> "field desc")
      )
    },
    test("support name annotation") {
      @name("SealedTrait")
      sealed trait St
      @name("className")
      case class Cfg(@name("otherName") fname: String) extends St

      def collectPath[T](desc: ConfigDescriptor[String, String, T]): List[String] = desc match {
        case ConfigDescriptor.Default(config, _)        => collectPath(config)
        case Describe(config, _)                        => collectPath(config)
        case ConfigDescriptor.DynamicMap(_, config)     => collectPath(config)
        case Nested(path, config)                       => path :: collectPath(config)
        case ConfigDescriptor.Optional(config)          => collectPath(config)
        case ConfigDescriptor.OrElse(left, right)       => collectPath(left) ::: collectPath(right)
        case ConfigDescriptor.OrElseEither(left, right) => collectPath(left) ::: collectPath(right)
        case ConfigDescriptor.Sequence(_, config)       => collectPath(config)
        case ConfigDescriptor.Source(_, _)              => Nil
        case ConfigDescriptor.Zip(left, right)          => collectPath(left) ::: collectPath(right)
        case ConfigDescriptor.XmapEither(config, _, _)  => collectPath(config)
      }

      assert(collectPath(descriptor[St]))(equalTo("SealedTrait" :: "className" :: "otherName" :: Nil))
    },
    test("support default value") {
      @describe("class desc")
      case class Cfg(fname: String = "defaultV")

      def collectDefault[T](
        desc: ConfigDescriptor[String, String, T],
        path: Option[String]
      ): List[(Option[String], Any)] = desc match {
        case ConfigDescriptor.Default(config, v)    => (path -> v) :: collectDefault(config, path)
        case Describe(config, _)                    => collectDefault(config, path)
        case ConfigDescriptor.DynamicMap(_, config) => collectDefault(config, path)
        case Nested(path, config)                   => collectDefault(config, Some(path))
        case ConfigDescriptor.Optional(config)      => collectDefault(config, path)
        case ConfigDescriptor.OrElse(left, right)   => collectDefault(left, path) ::: collectDefault(right, path)
        case ConfigDescriptor.OrElseEither(left, right) =>
          collectDefault(left, path) ::: collectDefault(right, path)
        case ConfigDescriptor.Sequence(_, config)      => collectDefault(config, path)
        case ConfigDescriptor.Source(_, _)             => Nil
        case ConfigDescriptor.Zip(left, right)         => collectDefault(left, path) ::: collectDefault(right, path)
        case ConfigDescriptor.XmapEither(config, _, _) => collectDefault(config, path)
      }

      assert(collectDefault(descriptor[Cfg], None))(equalTo((Some("fname"), "defaultV") :: Nil))
    },
    test("support lists recursive") {
      case class A1(a: List[String])
      case class A2(a: List[A1])
      case class A3(a: List[A2])
      case class A4(a: List[A3])
      case class A5(a: List[A4])

      def loop(depth: Int): PropertyTree[String, String] =
        if (depth > 0) Record(Map("a" -> Sequence(List(loop(depth - 1)))))
        else Leaf("str")

      val src = ConfigSource.fromPropertyTree(loop(5), "tree")

      val res = read(descriptor[A5] from src)

      assert(res)(isRight(anything))
    },
    test("support lists non-recursive") {
      import NonRecursiveDerivation.descriptor
      import NonRecursiveListHelper._

      def loop(depth: Int): PropertyTree[String, String] =
        if (depth > 0) Record(Map("a" -> Sequence(List(loop(depth - 1)))))
        else Leaf("str")

      val src = ConfigSource.fromPropertyTree(loop(5), "tree")

      val res = read(descriptor[A5] from src)

      assert(res)(isRight(anything))
    },
    test("support nested lists non-recursive") {
      import NonRecursiveDerivation.{ descriptor, Descriptor }

      case class A(a: List[String])
      implicit val cA: Descriptor[A] = descriptor[A]
      val _                          = cA
      case class B(a: List[List[List[List[List[List[List[List[List[List[A]]]]]]]]]])

      def loop(depth: Int): PropertyTree[String, String] =
        if (depth > 0) Sequence(List(loop(depth - 1)))
        else Record(Map("a" -> Sequence(List(Leaf("s")))))

      val src = ConfigSource.fromPropertyTree(Record(Map("a" -> loop(10))), "tree")

      val res = read(descriptor[B] from src)

      assert(res)(isRight(anything))
    },
    test("support nested lists recursive") {
      case class A(a: List[String])
      case class B(a: List[List[List[List[List[List[List[List[List[List[A]]]]]]]]]])

      def loop(depth: Int): PropertyTree[String, String] =
        if (depth > 0) Sequence(List(loop(depth - 1)))
        else Record(Map("a" -> Sequence(List(Leaf("s")))))

      val src = ConfigSource.fromPropertyTree(Record(Map("a" -> loop(10))), "tree")

      val res = read(descriptor[B] from src)

      assert(res)(isRight(anything))
    }
  )
}

object NonRecursiveListHelper {
  import NonRecursiveDerivation.{ descriptor, Descriptor }

  case class A1(a: List[String])
  implicit val cA1: Descriptor[A1] = descriptor[A1]
  case class A2(a: List[A1])
  implicit val cA2: Descriptor[A2] = descriptor[A2]
  case class A3(a: List[A2])
  implicit val cA3: Descriptor[A3] = descriptor[A3]
  case class A4(a: List[A3])
  implicit val cA4: Descriptor[A4] = descriptor[A4]
  case class A5(a: List[A4])

}
