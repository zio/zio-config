package zio.config.yaml

import zio.config._
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}
import zio.{Config, ConfigProvider}

object YamlConfigSpec extends ZIOSpecDefault {
  def spec: Spec[Any, Config.Error] = suite("YamlConfig")(
    test("Read a complex structure into a sealed trait") {
      case class Child(sum: List[Sum])

      sealed trait Sum                                          extends Product with Serializable
      case class A(a: String, h: List[Int])                     extends Sum
      case class B(b: Boolean, e: List[C], f: Map[String, Int]) extends Sum

      final case class C(d: Int) extends Sum

      val config =
        Config
          .listOf(
            "sum",
            ((Config.string("a").zip(Config.listOf("h", Config.int)).to[A].nested("A")) orElseEither
              (Config
                .boolean("b")
                .zip(Config.listOf("e", Config.int.to[C]).nested("d"))
                .zip(Config.table("f", Config.int))
                .to[B]
                .nested("B")))
              .map(_.merge)
          )
          .to[Child]

      val provider =
        ConfigProvider.fromYamlString(
          """|sum:
             |  - A:
             |      a: "str"
             |      h: []
             |  - B:
             |      b: false
             |      d:
             |        e:
             |          - 1
             |          - 2
             |      f:
             |        hi: 1
             |        bi: 2
             |
             |      """.stripMargin
        )

      val zio = provider.load(config)

      val expected = Child(List(A("str", Nil), B(false, List(C(1), C(2)), Map("hi" -> 1, "bi" -> 2))))

      assertZIO(zio.exit)(succeeds(equalTo(expected)))
    },
    test("save read yaml config") {
      final case class DataBaseConfig(url: String)
      val configDataBaseConfig: Config[DataBaseConfig] = Config.string("url").to[DataBaseConfig]

      val yamlConfig: String = s"""url: "some_url""""
      val result             = read(configDataBaseConfig from ConfigProvider.fromYamlStringZIO(yamlConfig))
      val expected           = DataBaseConfig("some_url")
      assertZIO(result)(equalTo(expected))
    }
  )
}
