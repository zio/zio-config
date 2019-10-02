package zio.config

import org.scalacheck.{ Gen, Properties }
import zio.config.testsupport.TestSupport

object ListAndOptionalTests extends Properties("List and options tests") with TestSupport {

  private val genId = genSymbol(1, 5).map(Id)

  private val genOverallConfig: Gen[OverallConfig] =
    Gen.option(genId).map(t => OverallConfig(t.map(t => Option(Option(t)))))

  val cId: Config[Id] = Config.string("kId").xmap(Id)(_.value)

  val cOverallConfig: Config[OverallConfig] =
    cId.optional.optional.optional.xmap(OverallConfig)(_.option)

  property("optional write") = forAllZIO(genOverallConfig) { p =>
    write(cOverallConfig).run
      .provide(p)
      .shouldBe(
        p.option
          .flatMap(t => t.flatMap(tt => tt.map(ttt => Map("kId" -> ttt.value))))
          .getOrElse(Map.empty[String, String])
      )
  }

  final case class Id(value: String) extends AnyVal
  final case class OverallConfig(option: Option[Option[Option[Id]]])

}
