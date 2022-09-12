package zio.config.magnolia

import zio.config._
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

object OverrideDerivationTest extends ZIOSpecDefault {
  def spec: Spec[Any, Any] = suite("OverrideDerivationTest")(
    test("simple config") {

      case class Cfg(fieldName: String)

      val res = write(descriptorWithoutClassNames[Cfg], Cfg("a"))

      assertZIO(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(descriptorWithoutClassNames[Cfg] from v))
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

      val res = write(descriptorWithoutClassNames[Outer], cfg)

      assertZIO(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(descriptorWithoutClassNames[Outer] from v))
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

      val res = write(descriptorWithClassNames[Outer], cfg)

      assertZIO(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(descriptorWithClassNames[Outer] from v))
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

      val res = write(descriptorForPureConfig[Outer], cfg)

      assertZIO(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(descriptorForPureConfig[Outer] from v))
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

      val res = write(descriptorWithClassNames[Outer]("type"), cfg)

      assertZIO(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(descriptorWithClassNames[Outer]("type") from v))
      )(
        equalTo(cfg)
      )
    }
  )
}
