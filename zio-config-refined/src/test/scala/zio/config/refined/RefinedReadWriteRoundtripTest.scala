package zio.config.refined

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import zio.ZIO
import zio.config.ConfigDescriptor._
import zio.config.helpers._
import zio.config.refined.RefinedReadWriteRoundtripTestUtils._
import zio.config.{ read, write, BaseSpec, ConfigDescriptor, ConfigSource }
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object RefinedReadWriteRoundtripTest
    extends BaseSpec(
      suite("Refined support")(
        testM("Refined config roundtrip") {
          checkM(genRefinedProd) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(prodConfig(p.longs.value.size), p))
                reread  <- read(prodConfig(p.longs.value.size) from ConfigSource.fromPropertyTree(written))
              } yield reread

            assertM(p2, equalTo(p))
          }
        }
      )
    )

// TODO check unhappy paths

object RefinedReadWriteRoundtripTestUtils {

  case class RefinedProd(
    ldap: Refined[String, NonEmpty],
    port: Refined[Int, GreaterEqual[W.`1024`.T]],
    dburl: Option[Refined[String, NonEmpty]],
    longs: Refined[List[Long], Size[Greater[W.`2`.T]]]
  )

  def longList(n: Int): List[ConfigDescriptor[String, String, Long]] =
    (1 to n).toList
      .map(group => long(s"GROUP${group}_LONGVAL"))

  def longs(n: Int): ConfigDescriptor[String, String, List[Long]] =
    ConfigDescriptor.collectAll[String, String, Long](longList(n))

  def prodConfig(n: Int): ConfigDescriptor[String, String, RefinedProd] =
    (
      nonEmpty(string("LDAP")) |@|
        greaterEqual[W.`1024`.T](int("PORT")) |@|
        nonEmpty(string("DB_URL")).optional |@|
        size[Greater[W.`2`.T]](longs(n))
    )(
      RefinedProd.apply,
      RefinedProd.unapply
    )

  ////

  def genRefinedProd: Gen[Random, RefinedProd] =
    for {
      ldap  <- genSymbol(1, 10)
      port  <- Gen.int(1025, 64000)
      dburl <- Gen.option(genSymbol(1, 20))
      n     <- Gen.int(3, 10)
      longs <- Gen.listOfN(n)(Gen.anyLong)
    } yield RefinedProd(
      Refined.unsafeApply(ldap),
      Refined.unsafeApply(port),
      dburl.map(Refined.unsafeApply),
      Refined.unsafeApply(longs)
    )

}
