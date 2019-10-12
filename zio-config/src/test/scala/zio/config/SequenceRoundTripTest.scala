package zio.config

import org.scalacheck.{ Gen, Properties }
import zio.ZIO
import zio.config.testsupport.TestSupport
import zio.config.Config._

object SequenceRoundTripTest extends Properties("sequence round trip tests") with TestSupport {

  private val genId = genSymbol(1, 5).map(Id)

  val key: (Int, Int) => String =
    (n1, n2) => s"GROUP${n1}_id_${n2}"

  val cId: String => ConfigDescriptor[Id] =
    string(_).xmap(Id)(_.value)

  private val genOverallConfig: Gen[Map[String, String]] =
    for {
      optId1 <- Gen.option(genId)
      id2    <- genId
      n      <- Gen.oneOf(1, 10, 100)
    } yield (0 to n)
      .flatMap(
        nn =>
          List(key(nn, 2) -> id2.value) ++
            optId1.toList.flatMap(id1 => List(key(nn, 1) -> id1.value))
      )
      .toMap

  property("optional write") = forAllZIO(genOverallConfig) { p =>
    val config: ConfigDescriptor[List[OverallConfig]] =
      ConfigDescriptor.sequence(
        p.toList.map(prefix => (cId(prefix._1).optional |@| cId(prefix._1))(OverallConfig.apply, OverallConfig.unapply))
      )

    val readAndWrite =
      for {
        result  <- read(config)
        written <- ZIO.effectTotal(write(config, result))
      } yield written

    readAndWrite
      .map(_.map(_.flatten()))
      .provide(ConfigSource.fromMap(p))
      .map(_.fold(_ => Nil, t => t.toList.sortBy(_._1)))
      .shouldBe(p.toList.sortBy(_._1))
  }

  final case class Id(value: String) extends AnyVal

  final case class OverallConfig(id1: Option[Id], id2: Id)

}
