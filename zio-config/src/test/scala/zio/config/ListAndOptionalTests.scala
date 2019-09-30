package zio.config

import org.scalacheck.{ Gen, Properties }
import zio.config.testsupport.TestSupport

object ListAndOptionalTests extends Properties("List and options tests") with TestSupport {

  private val genId = genSymbol(1, 5).map(Id)

  private val genOverallConfig =
    Gen.option(genId).map(OverallConfig)

  val cId: Config[Id] = string("kId").xmap(Id)(_.value)

  val cOverallConfig: Config[OverallConfig] =
    cId.optional.xmap(OverallConfig)(_.list)

  property("optional write") = forAllZIO(genOverallConfig) { p =>
    write(cOverallConfig).run
      .provide(p)
      .shouldBe(p.list.map(t => Map("kId" -> t.value)).getOrElse(Map.empty[String, String]))
  }

  final case class Id(value: String) extends AnyVal
  final case class OverallConfig(list: Option[Id])

}
