package zio.config

import zio.random.Random
import zio.test.Gen

object helpers {
  final case class CoproductConfig(coproduct: Either[DataItem, NestedConfig])
  final case class DataItem(oid: Option[Id], count: Int)
  final case class DbUrl(value: String) extends AnyVal
  final case class EnterpriseAuth(id: Id, dburl: DbUrl)
  final case class Id(value: String) extends AnyVal
  final case class NestedConfig(enterpriseAuth: EnterpriseAuth, count: Int, factor: Float)
  final case class OverallConfig(id1: Option[Id], id2: Id)

  def genSymbol(min: Int, max: Int): Gen[Random, String] =
    for {
      n <- Gen.int(min, max)
      s <- Gen.listOfN(n)(Gen.alphaNumericChar)
    } yield s.mkString

  def genNonEmptyString(length: Int): Gen[Random, String] = genSymbol(1, length)

  val genId: Gen[Random, Id] = genSymbol(1, 5).map(Id(_))

  val genDataItem: Gen[Random, DataItem] =
    for {
      oid   <- Gen.option(genId)
      count <- Gen.anyInt
    } yield DataItem(oid, count)

  val genDbUrl: Gen[Random, DbUrl] = genNonEmptyString(20).map(DbUrl)

  val genEnterpriseAuth: Gen[Random, EnterpriseAuth] =
    for {
      id    <- genId
      dburl <- genDbUrl
    } yield EnterpriseAuth(id, dburl)

  val genNestedConfig: Gen[Random, NestedConfig] =
    for {
      auth   <- genEnterpriseAuth
      count  <- Gen.anyInt
      factor <- Gen.anyFloat
    } yield NestedConfig(auth, count, factor)

  val genCoproductConfig: Gen[Random, CoproductConfig] =
    Gen.either(genDataItem, genNestedConfig).map(CoproductConfig)

  val genOverallConfig: Gen[Random, Map[String, String]] = {
    def makePair(id: Int, idx: Int, value: String): (String, String) =
      s"GROUP${id}_id_${idx}" -> value

    for {
      optId1 <- Gen.option(genId)
      id2    <- genId
      n      <- Gen.oneOf(Gen.const(1), Gen.const(10), Gen.const(100))
    } yield (0 to n).flatMap { nn =>
      val pair = makePair(nn, 2, id2.value)
      optId1.fold(List(pair))(id => List(pair, makePair(nn, 1, id.value)))
    }.toMap
  }
}
