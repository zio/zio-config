package zio.config

import zio.config.Config._
import zio.config.helpers._
import zio.config.ListAndOptionalTestsUtils._
import zio.test._
import zio.test.Assertion._

object ListAndOptionalTests
    extends BaseSpec(suite("List and options tests")(testM("optional write") {
      checkM(genOverallConfig) { p =>
        val actual = write(cOverallConfig).provide(p)

        val expected = p.option
          .flatMap(t => t.flatMap(tt => tt.map(ttt => Map("kId" -> ttt.value))))
          .getOrElse(Map.empty[String, String])

        assertM(actual, equalTo(expected))
      }
    }))

object ListAndOptionalTestsUtils {
  final case class OverallConfig(option: Option[Option[Option[Id]]])

  val genOverallConfig =
    Gen.option(genId).map(t => OverallConfig(t.map(t => Option(Option(t)))))

  private val cId = string("kId").xmap(Id)(_.value)

  val cOverallConfig =
    cId.optional.optional.optional.xmap(OverallConfig)(_.option)
}
