package zio.config

import zio.ZIO
import zio.config.ConfigDescriptor._
import zio.config.ListAndOptionalTestUtils._
import zio.config.helpers._
import zio.test.Assertion._
import zio.test._

object ListAndOptionalTest
    extends BaseSpec(suite("List and options")(testM("optional write") {
      checkM(genOverallConfig) { p =>
        val actual = ZIO.fromEither(write(cOverallConfig, p)).map(_.flattenString())

        val expected = p.option
          .flatMap(t => t.flatMap(tt => tt.map(ttt => Map("kId" -> singleton(ttt.value)))))
          .getOrElse(Map.empty[String, ::[String]])

        assertM(actual)(equalTo(expected))
      }
    }))

object ListAndOptionalTestUtils {
  final case class OverallConfig(option: Option[Option[Option[Id]]])

  val genOverallConfig =
    Gen.option(genId).map(t => OverallConfig(t.map(t => Option(Option(t)))))

  private val cId = string("kId")(Id.apply, Id.unapply)

  val cOverallConfig =
    cId.optional.optional.optional(OverallConfig.apply, OverallConfig.unapply)
}
