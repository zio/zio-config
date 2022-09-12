package zio.config.magnolia

import zio.config.PropertyTree.{Leaf, Record}
import zio.config.{PropertyTree, _}
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

import ConfigDescriptorAdt._

object DerivationTest extends ZIOSpecDefault {
  def spec: Spec[Any, ReadError[String]] = suite("DerivationTest")(
    test("support describe annotation") {
      @describe("class desc")
      case class Cfg(@describe("field desc") fname: String)

      def collectDescriptions[T](
        desc: ConfigDescriptor[T],
        path: Option[String]
      ): List[(Option[String], String)] = desc match {
        case Lazy(thunk)                   => collectDescriptions(thunk(), path)
        case Default(config, _)            => collectDescriptions(config, path)
        case DynamicMap(config)            => collectDescriptions(config, path)
        case Describe(config, message)     => (path, message) :: collectDescriptions(config, path)
        case Nested(path, config, _)       => collectDescriptions(config, Some(path))
        case Optional(config)              => collectDescriptions(config, path)
        case OrElse(left, right)           =>
          collectDescriptions(left, path) ::: collectDescriptions(right, path)
        case OrElseEither(left, right)     =>
          collectDescriptions(left, path) ::: collectDescriptions(right, path)
        case Sequence(config)              => collectDescriptions(config, path)
        case Source(_, _)                  => Nil
        case Zip(left, right)              =>
          collectDescriptions(left, path) ::: collectDescriptions(right, path)
        case TransformOrFail(config, _, _) => collectDescriptions(config, path)
      }

      assert(collectDescriptions(descriptor[Cfg], None))(
        contains((None: Option[String]) -> "class desc") &&
          contains(None                 -> "field desc")
      )
    },
    test("support name annotation") {

      @name("St")
      sealed trait SealedTrait
      @name("className")
      case class Cfg(@name("otherName") fname: String) extends SealedTrait

      def collectPath[T](desc: ConfigDescriptor[T]): List[String] = desc match {
        case Lazy(thunk)                   => collectPath(thunk())
        case Default(config, _)            => collectPath(config)
        case Describe(config, _)           => collectPath(config)
        case DynamicMap(config)            => collectPath(config)
        case Nested(path, config, _)       => path :: collectPath(config)
        case Optional(config)              => collectPath(config)
        case OrElse(left, right)           => collectPath(left) ::: collectPath(right)
        case OrElseEither(left, right)     => collectPath(left) ::: collectPath(right)
        case Sequence(config)              => collectPath(config)
        case Source(_, _)                  => Nil
        case Zip(left, right)              => collectPath(left) ::: collectPath(right)
        case TransformOrFail(config, _, _) => collectPath(config)
      }

      // IntelliJ will hide this, however it is required

      assert(collectPath(descriptorWithClassNames[SealedTrait]))(
        equalTo("St" :: "className" :: "otherName" :: Nil)
      )
    },
    test("support names annotation") {

      @names("St1", "St2")
      sealed trait SealedTrait
      @names("className1", "className2")
      case class Cfg(@names("otherName1", "otherName2") fname: String) extends SealedTrait

      val desc = descriptorWithClassNames[SealedTrait]

      def collectPath[T](desc: ConfigDescriptor[T]): List[String] = desc match {
        case Lazy(thunk)                   => collectPath(thunk())
        case Default(config, _)            => collectPath(config)
        case Describe(config, _)           => collectPath(config)
        case DynamicMap(config)            => collectPath(config)
        case Nested(path, config, _)       => path :: collectPath(config)
        case Optional(config)              => collectPath(config)
        case OrElse(left, right)           => collectPath(left) ::: collectPath(right)
        case OrElseEither(left, right)     => collectPath(left) ::: collectPath(right)
        case Sequence(config)              => collectPath(config)
        case Source(_, _)                  => Nil
        case Zip(left, right)              => collectPath(left) ::: collectPath(right)
        case TransformOrFail(config, _, _) => collectPath(config)
      }

      def cross[A](a: List[A], b: List[A]) =
        for {
          x <- a
        } yield x :: b

      val cfgRes =
        cross(("className1" :: "className2" :: "Cfg" :: Nil), ("otherName1" :: "otherName2" :: "fname" :: Nil)).flatten

      assert(collectPath(desc))(equalTo(cross(("St1" :: "St2" :: "SealedTrait" :: Nil), cfgRes).flatten))
    },
    test("support default value") {
      @describe("class desc")
      case class Cfg(fname: String = "defaultV")

      def collectDefault[T](
        desc: ConfigDescriptor[T],
        path: Option[String]
      ): List[(Option[String], Any)] = desc match {
        case Lazy(thunk)                   => collectDefault(thunk(), path)
        case Default(config, v)            => (path -> v) :: collectDefault(config, path)
        case Describe(config, _)           => collectDefault(config, path)
        case DynamicMap(config)            => collectDefault(config, path)
        case Nested(path, config, _)       => collectDefault(config, Some(path))
        case Optional(config)              => collectDefault(config, path)
        case OrElse(left, right)           => collectDefault(left, path) ::: collectDefault(right, path)
        case OrElseEither(left, right)     =>
          collectDefault(left, path) ::: collectDefault(right, path)
        case Sequence(config)              => collectDefault(config, path)
        case Source(_, _)                  => Nil
        case Zip(left, right)              => collectDefault(left, path) ::: collectDefault(right, path)
        case TransformOrFail(config, _, _) => collectDefault(config, path)
      }

      assert(collectDefault(descriptor[Cfg], None))(equalTo((None, "defaultV") :: Nil))
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

      val src = ConfigSource.fromPropertyTree(loop(5), "tree")

      val res = read(descriptor[A5] from src)

      assertZIO(res.either)(isRight(anything))
    },
    test("support nested lists recursive") {
      case class A(a: List[String])
      case class B(a: List[List[List[List[List[List[List[List[List[List[A]]]]]]]]]])

      def loop(depth: Int): PropertyTree[String, String] =
        if (depth > 0) PropertyTree.Sequence(List(loop(depth - 1)))
        else Record(Map("a" -> PropertyTree.Sequence(List(Leaf("s")))))

      val src                                            =
        ConfigSource.fromPropertyTree(Record(Map("a" -> loop(10))), "tree")
      val res                                            =
        read(descriptor[B] from src)

      assertZIO(res.either)(isRight(anything))
    },
    test("support recursive structures") {
      case class SimpleRec(id: Int, nested: Option[SimpleRec])

      val desc                                         = descriptor[SimpleRec]
      val simpleTestTree: PropertyTree[String, String] = PropertyTree.Record(
        Map(
          "id"     -> PropertyTree.Leaf("1"),
          "nested" -> PropertyTree.Record(
            Map(
              "id" -> PropertyTree.Leaf("2")
            )
          )
        )
      )
      val simpleTestSource: ConfigSource               = ConfigSource.fromPropertyTree(
        simpleTestTree,
        "tree"
      )

      val simpleRecursiveValue: SimpleRec = SimpleRec(1, Some(SimpleRec(2, None)))
      assertZIO(read(desc from simpleTestSource))(equalTo(simpleRecursiveValue))
    }
  )
}
