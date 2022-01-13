package zio.config.typesafe

import zio.config.{read, _}
import zio.test.Assertion._
import zio.test.{Spec, TestFailure, TestSuccess, ZIOSpecDefault, assertM}

import ConfigDescriptor._

object ListOrSingletonSpec extends ZIOSpecDefault {
  override def spec: Spec[Any, TestFailure[ReadError[String]], TestSuccess] =
    suite("listOrSingleton")(
      test("reads singleton") {
        val configString =
          """list = "x"
            |""".stripMargin

        val config = listOrSingleton("list")(string)
        val source = ConfigSource.fromHoconString(configString)

        assertM(read(config from source))(equalTo(List("x")))
      },
      test("reads list") {
        val configString =
          """list = ["x", "y"]
            |""".stripMargin

        val config = listOrSingleton("list")(string)
        val source = ConfigSource.fromHoconString(configString)

        assertM(read(config from source))(equalTo(List("x", "y")))
      }
    )
}
