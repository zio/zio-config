package zio.config.pureconfig

import zio.config._
import zio.test.Assertion._
import zio.test._
import zio.{ConfigProvider, Scope, ZIO}

object PureconfigTest extends BaseSpec {
  override val spec: Spec[TestEnvironment with Scope, Any] =
    suite("Pureconfig support")(
      test("empty vector") {
        val pureConfig = fromPureconfig[Vector[String]]
        val v2         =
          for {
            provider         <- ZIO.succeed(ConfigProvider.fromMap(Map("k" -> "[]")))
            pureConfigResult <- provider.load(pureConfig.nested("k"))
          } yield pureConfigResult

        assertZIO(v2)(equalTo(Vector.empty))
      },
      test("nonEmpty vector") {
        val pureConfig = fromPureconfig[Vector[String]]
        val v2         =
          for {
            provider         <- ZIO.succeed(ConfigProvider.fromMap(Map("k" -> "[a, b, c]")))
            pureConfigResult <- provider.load(pureConfig.nested("k"))
          } yield pureConfigResult

        assertZIO(v2)(equalTo(Vector("a", "b", "c")))
      }
    )
}
