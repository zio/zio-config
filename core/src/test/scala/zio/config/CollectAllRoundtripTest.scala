package zio.config

import zio.ZIO
import zio.config.ConfigDescriptor._
import zio.config.helpers._
import zio.config.SequenceRoundtripTestUtils._
import zio.random.Random
import zio.test._
import zio.test.Assertion._

object CollectAllRoundtripTest
    extends BaseSpec(
      suite("sequence round trip")(
        testM("optional write") {
          checkM(genOverallConfig) {
            p =>
              val cId: String => ConfigDescriptor[String, String, Id] = string(_).xmap(Id)(_.value)

              // List is nonempty
              val consOfConfig = {
                val configs = p.toList.map(
                  prefix => (cId(prefix._1).optional |@| cId(prefix._1))(OverallConfig.apply, OverallConfig.unapply)
                )
                ::(configs.head, configs.tail)
              }

              val config =
                ConfigDescriptor.sequence(
                  consOfConfig
                )

              val readAndWrite
                : ZIO[Any, ReadErrors[Vector[String], String], Either[String, PropertyTree[String, String]]] =
                for {
                  result  <- read(config from ConfigSource.fromMap(p))
                  written <- ZIO.effectTotal(write(config, result))
                } yield written

              val actual = readAndWrite
                .map(_.map(_.flattenString()))
                .map(_.fold(_ => Nil, _.toList.sortBy(_._1)))

              assertM(actual, equalTo(p.toList.sortBy(_._1).map({ case (k, v) => (k, singleton(v)) })))
          }
        }
      )
    )

object SequenceRoundtripTestUtils {
  final case class OverallConfig(id1: Option[Id], id2: Id)

  val genOverallConfig: Gen[Random, Map[String, String]] =
    for {
      optId1 <- Gen.option(genId)
      id2    <- genId
      n      <- Gen.oneOf(Gen.const(1), Gen.const(10), Gen.const(100))
    } yield rangeMap(optId1, id2, n)

  private def rangeMap(optId1: Option[Id], id2: Id, n: Int): Map[String, String] =
    (1 to n).flatMap { nn =>
      val pair = makePair(nn, 2, id2.value)
      optId1.fold(List(pair))(id => List(pair, makePair(nn, 1, id.value)))
    }.toMap

  private def makePair(id: Int, idx: Int, value: String): (String, String) =
    s"GROUP${id}_id_${idx}" -> value

}
