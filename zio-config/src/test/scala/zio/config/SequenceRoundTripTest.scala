package zio.config

import org.scalacheck.{ Gen, Properties }
import zio.ZIO
import zio.config.testsupport.TestSupport

object SequenceRoundTripTest extends Properties("sequence round trip tests") with TestSupport {

  private val genId = genSymbol(1, 5).map(Id)

  val key: (Int, Int) => String =
    (n1, n2) => s"GROUP${n1}_id_${n2}"

  val cId: String => Config[Id] =
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
    val config: Config[List[OverallConfig]] =
      Config.sequence(
        p.toList.map(prefix => (opt(cId(prefix._1)) <*> cId(prefix._1))(OverallConfig.apply, OverallConfig.unapply))
      )

    val readAndWrite: ZIO[ConfigSource, ReadErrors, Either[String, Map[String, String]]] =
      for {
        result    <- read(config).run
        (_, conf) = result
        written   <- write(config).run.provide(conf).either
      } yield written

    readAndWrite
      .provide(mapSource(p))
      .map(_.fold(_ => Nil, t => t.toList.sortBy(_._1)))
      .shouldBe(p.toList.sortBy(_._1))
  }

  final case class Id(value: String) extends AnyVal

  final case class OverallConfig(id1: Option[Id], id2: Id)

}
