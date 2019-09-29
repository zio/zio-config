package zio.config

import org.scalacheck.Properties
import zio.config.testsupport.TestSupport

object OperationsTest extends Properties("Simple operations") with TestSupport {

  val genId = genSymbol(1, 5).map(Id)
  val genOverallConfig =
    for {
      id     <- genId
      isNone <- genBoolean
    } yield OverallConfig(if (isNone) None else Some(id))

  val cId: Config[Id] = string("kId").xmap(Id)(_.value)
  val cOverallConfig: Config[OverallConfig] =
    (opt(cId))
      .xmap(OverallConfig)(_.opt)

  property("opt write") = forAllZIO(genOverallConfig) { p =>
    write(cOverallConfig).run
      .provide(p)
      .shouldBe(KeyValue(Map("kId" -> p.opt.map(_.value).mkString(","))))
  }

  ////

  final case class Id(value: String) extends AnyVal
  final case class OverallConfig(opt: Option[Id])

}
