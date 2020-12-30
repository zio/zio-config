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
      suite("ConfigDescriptor.collectAll")(
        testM("Can convert a list of config-descriptor to a single config-descriptor that returns list") {
          checkM(generateListOfGroups) {
            groups =>
              val cId: String => ConfigDescriptor[Id] = string(_)(Id.apply, Id.unapply)

              // List is nonempty
              val consOfConfig = {
                val configs = groups.map(
                  group =>
                    (cId(group.id1Key).optional |@| cId(group.id2Key))(IdentityDetails.apply, IdentityDetails.unapply)
                )
                ::(configs.head, configs.tail)
              }

              val inputSource: Map[String, String] =
                groups.flatMap(_.toMap.toList).toMap

              val config = collectAll(consOfConfig.head, consOfConfig.tail: _*)

              val readAndWrite: ZIO[Any, Any, PropertyTree[String, String]] =
                for {
                  result  <- ZIO.fromEither(read(config from ConfigSource.fromMap(inputSource)))
                  written <- ZIO.fromEither(write(config, result))
                } yield written

              val actual = readAndWrite
                .map(_.flattenString())
                .fold(_ => Nil, _.toList.sortBy(_._1))

              assertM(actual)(equalTo(inputSource.toList.sortBy(_._1).map({ case (k, v) => (k, singleton(v)) })))
          }
        }
      )
    )

object SequenceRoundtripTestUtils {
  final case class IdentityDetails(id1: Option[Id], id2: Id)

  final case class Group(number: Int, identityDetails: IdentityDetails) {
    val id1Key =
      s"GROUP_${number}_id_1"

    val id2Key =
      s"GROUP_${number}_id_2"

    val toMap: Map[String, String] =
      Map(id2Key -> identityDetails.id2.value) ++
        identityDetails.id1.fold(Map.empty[String, String])(v => Map(id1Key -> v.value))
  }

  val generateListOfGroups: Gen[Random, List[Group]] =
    for {
      optId1 <- Gen.option(genId)
      id2    <- genId
      n      <- Gen.oneOf(Gen.const(1), Gen.const(10))
    } yield rangeMap(n, IdentityDetails(optId1, id2))

  val generateGroupMap: Gen[Random, Map[String, String]] =
    generateListOfGroups.map(_.flatMap(_.toMap.toList).toMap)

  private def rangeMap(totalGroups: Int, overallConfig: IdentityDetails): List[Group] =
    (1 to totalGroups).toList.map { groupNumber =>
      Group(groupNumber, overallConfig)
    }
}
