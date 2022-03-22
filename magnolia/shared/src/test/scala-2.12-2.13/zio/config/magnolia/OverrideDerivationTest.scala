package zio.config.magnolia

import zio.config._
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

object OverrideDerivationTest extends ZIOSpecDefault {
  def spec: Spec[Any, TestFailure[Serializable], TestSuccess] = suite("OverrideDerivationTest")(
    test("simple config") {

      case class Cfg(fieldName: String)

      val res = write(Descriptor.descriptorWithoutClassNames[Cfg], Cfg("a"))

      assertM(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(Descriptor.descriptorWithoutClassNames[Cfg] from v))
      )(
        equalTo(Cfg("a"))
      )
    },
    test("unwrapped sealed hierarchy") {

      sealed trait Inner
      case object Obj1Name                     extends Inner
      case object OtherOBJECT                  extends Inner
      case class ClassWithData(data: String)   extends Inner
      case class ClassWithValue(value: String) extends Inner

      case class Outer(list: List[Inner])

      val cfg = Outer(List(OtherOBJECT, Obj1Name, ClassWithValue("a"), ClassWithData("b")))

      val res = write(Descriptor.descriptorWithoutClassNames[Outer], cfg)

      assertM(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(Descriptor.descriptorWithoutClassNames[Outer] from v))
      )(
        equalTo(cfg)
      )
    },
    test("wrapped sealed hierarchy") {

      sealed trait Inner
      case object Obj1Name                     extends Inner
      case object OtherOBJECT                  extends Inner
      case class ClassWithData(data: String)   extends Inner
      case class ClassWithValue(value: String) extends Inner

      case class Outer(list: List[Inner])

      val cfg = Outer(List(OtherOBJECT, Obj1Name, ClassWithValue("a"), ClassWithData("b")))

      val res = write(Descriptor.descriptorWithClassNames[Outer], cfg)

      assertM(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(Descriptor.descriptorWithClassNames[Outer] from v))
      )(
        equalTo(cfg)
      )
    },
    test("config with type labels ignoring the name of sealed trait") {

      sealed trait Inner
      case object Obj1Name                     extends Inner
      case object OtherOBJECT                  extends Inner
      case class ClassWithData(data: String)   extends Inner
      case class ClassWithValue(value: String) extends Inner

      case class Outer(list: List[Inner])

      val cfg = Outer(List(OtherOBJECT, Obj1Name, ClassWithValue("a"), ClassWithData("b")))

      val res = write(Descriptor.descriptorForPureConfig[Outer], cfg)

      assertM(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(Descriptor.descriptorForPureConfig[Outer] from v))
      )(
        equalTo(cfg)
      )
    },
    test("config with type labels wrapped with sealed trait name") {

      sealed trait Inner
      case object Obj1Name                     extends Inner
      case object OtherOBJECT                  extends Inner
      case class ClassWithData(data: String)   extends Inner
      case class ClassWithValue(value: String) extends Inner

      case class Outer(list: List[Inner])

      val cfg = Outer(List(OtherOBJECT, Obj1Name, ClassWithValue("a"), ClassWithData("b")))

      val res = write(Descriptor.descriptorWithClassesWithLabel[Outer]("type"), cfg)

      assertM(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(Descriptor.descriptorWithClassesWithLabel[Outer]("type") from v))
      )(
        equalTo(cfg)
      )
    }
  )
}
