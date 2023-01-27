package zio.config.typesafe

import zio.config.{read, _}
import zio.test.Assertion._
import zio.test.{Spec, ZIOSpecDefault, assertZIO}

import Config._

object ListOrSingletonSpec extends ZIOSpecDefault {
  override def spec: Spec[Any, Config.Error] =
    suite("listOrSingleton")(
      test("reads singleton") {
        val configString =
          """list = "x"
            |""".stripMargin

        val config = listOrSingleton("list")(string)
        val source = ConfigSource.fromHoconString(configString)

        assertZIO(read(config from source))(equalTo(List("x")))
      },
      test("reads list") {
        val configString =
          """list = ["x", "y"]
            |""".stripMargin

        val config = listOrSingleton("list")(string)
        val source = ConfigSource.fromHoconString(configString)

        assertZIO(read(config from source))(equalTo(List("x", "y")))
      }
    )
}
