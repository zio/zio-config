package zio.config.magnolia

import zio.config._
import zio.test.Assertion._
import zio.test._

object OverrideDerivationTestEnv extends DeriveConfigDescriptor {
  import Descriptor.SealedTraitStrategy._

  override def mapClassName(name: String): String = toSnakeCase(name) + "_suffix"
  override def mapFieldName(name: String): String = "prefix_" + toSnakeCase(name)

  override def sealedTraitStrategy: Descriptor.SealedTraitStrategy =
    ignoreSealedTraitName && ignoreSubClassName
}

object OverrideDerivationTest extends DefaultRunnableSpec {
  val spec: ZSpec[Environment, Failure] = suite("OverrideDerivationTest")(
    testM("simple config") {
      import OverrideDerivationTestEnv._

      case class Cfg(fieldName: String)

      val res = write(getDescriptor[Cfg].desc, Cfg("a"))

      assertM(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(getDescriptor[Cfg].desc from v))
      )(
        equalTo(Cfg("a"))
      )
    },
    testM("unwrapped sealed hierarchy") {
      import OverrideDerivationTestEnv._

      sealed trait Inner
      case object Obj1Name                     extends Inner
      case object OtherOBJECT                  extends Inner
      case class ClassWithData(data: String)   extends Inner
      case class ClassWithValue(value: String) extends Inner

      case class Outer(list: List[Inner])

      val cfg = Outer(List(OtherOBJECT, Obj1Name, ClassWithValue("a"), ClassWithData("b")))

      val res = write(OverrideDerivationTestEnv.getDescriptor[Outer].desc, cfg)

      assertM(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(getDescriptor[Outer].desc from v))
      )(
        equalTo(cfg)
      )
    },
    testM("wrapped sealed hierarchy") {
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

      val res = write(wrappedSealedHierarchy.descriptor[Outer], cfg)

      assertM(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(wrappedSealedHierarchy.descriptor[Outer] from v))
      )(
        equalTo(cfg)
      )
    },
    testM("config with type labels ignoring the name of sealed trait") {
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

      val res = write(wrappedSealedHierarchy.descriptor[Outer], cfg)

      assertM(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(wrappedSealedHierarchy.descriptor[Outer] from v))
      )(
        equalTo(cfg)
      )
    },
    testM("config with type labels wrapped with sealed trait name") {
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

      val res = write(wrappedSealedHierarchy.descriptor[Outer], cfg)

      assertM(
        zio.ZIO
          .fromEither(res)
          .map(ConfigSource.fromPropertyTree(_, "tree"))
          .flatMap(v => read(wrappedSealedHierarchy.descriptor[Outer] from v))
      )(
        equalTo(cfg)
      )
    }
  )
}
