package zio.config

import org.scalacheck.{ Gen, Properties }
import zio.config.testsupport.TestSupport

object ListAndOptionalTests extends Properties("Simple operations") with TestSupport {

  val genId = genSymbol(1, 5).map(Id)
  val genOverallConfig =
    for {
      ids <- Gen.option(genId)
    } yield OverallConfig(ids)

  val cId: Config[Id] = string("kId").xmap(Id)(_.value)
  val cOverallConfig: Config[OverallConfig] =
    opt(cId).xmap(OverallConfig)(_.list)

  property("optional write") = forAllZIO(genOverallConfig) { p =>
    write(cOverallConfig).run
      .provide(p)
      .shouldBe(KeyValue(p.list.map(t => Map("kId" -> t.value)).getOrElse(Map.empty[String, String])))
  }

  final case class Id(value: String) extends AnyVal
  final case class OverallConfig(list: Option[Id])

}
