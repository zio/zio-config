package zio.config.typesafe

import zio.config._, ConfigDescriptor._
import zio.config.read
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test.{DefaultRunnableSpec, ZSpec, assertM}

object ListOrSingletonSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("listOrSingleton")(
      testM("reads singleton") {
        val configString =
          """list = "x"
            |""".stripMargin

        val config = listOrSingleton("list")(string)
        val source = ConfigSource.fromHoconString(configString)

        assertM(read(config from source))(equalTo(List("x")))
      },
      testM("reads list") {
        val configString =
          """list = ["x", "y"]
            |""".stripMargin

        val config = listOrSingleton("list")(string)
        val source = ConfigSource.fromHoconString(configString)

        assertM(read(config from source))(equalTo(List("x", "y")))
      }
    )
}
