package zio.config.yaml

import zio.Runtime.default
import zio.{Config, ConfigProvider, Unsafe, ZIO}
import zio.config._
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

object YamlConfigSpec extends ZIOSpecDefault {
  def spec: Spec[Any, Config.Error] = suite("YamlConfig")(
    test("Read a complex structure into a sealed trait") {
      case class Child(sum: List[Sum])

      sealed trait Sum         extends Product with Serializable
      case class A(a: String)  extends Sum
      case class B(b: Boolean) extends Sum

      val config =
        Config
          .listOf(
            "sum",
            ((Config.string("a").to[A].nested("A")) orElseEither
              (Config.boolean("b").to[B].nested("B")))
              .map(_.merge)
          )
          .to[Child]

      val provider =
        ConfigProvider.fromYamlString(
          """|sum:
             |  - A:
             |      a: "str"
             |  - B:
             |      b: false""".stripMargin
        )

      val zio = provider.load(config)

      val expected = Child(List(A("str"), B(false)))

      assertZIO(zio.exit)(succeeds(equalTo(expected)))
    }
  )
}
