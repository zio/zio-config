package zio.config.typesafe

import zio.ZIO
import zio.config.ConfigDescriptor._
import zio.config.read
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test.{DefaultRunnableSpec, ZSpec, assertM}

object ListOrSingletonSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("listOrSingleton")(
      test("reads singleton") {
        val configString =
          """list = "x"
            |""".stripMargin

        val config = listOrSingleton("list")(string)
        val result = ZIO.fromEither(
          for {
            src    <- TypesafeConfigSource.fromHoconString(configString)
            result <- read(config from src)
          } yield result
        )

        assertM(result)(equalTo(List("x")))
      },
      test("reads list") {
        val configString =
          """list = ["x", "y"]
            |""".stripMargin

        val config = listOrSingleton("list")(string)
        val result = ZIO.fromEither(
          for {
            src    <- TypesafeConfigSource.fromHoconString(configString)
            result <- read(config from src)
          } yield result
        )

        assertM(result)(equalTo(List("x", "y")))
      }
    )
}
