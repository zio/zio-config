package zio.config.refined

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string.Trimmed
import zio.ZIO
import zio.config.helpers._
import zio.config.refined.RefinedReadWriteRoundtripTestUtils._
import zio.config.{ helpers, read, write, BaseSpec, ConfigDescriptor, ConfigSource }
import ConfigDescriptor._
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object RefinedReadWriteRoundtripTest
    extends BaseSpec(
      suite("Refined support")(
        testM("Refined config roundtrip") {
          checkM(genRefinedProd) { p =>
            val cfg = prodConfig(p.longs.value.size)
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread  <- ZIO.fromEither(read(cfg from ConfigSource.fromPropertyTree(written, "tree")))
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config invalid") {
          check(genRefinedProdInvalid) {
            case (n, envMap) =>
              val p2 =
                read(prodConfig(n) from ConfigSource.fromMap(envMap))

              assert(p2)(helpers.isErrors(hasField("size", _.size, equalTo(5))))
          }
        }
      )
    )

object RefinedReadWriteRoundtripTestUtils {

  case class RefinedProd(
    ldap: Refined[String, NonEmpty],
    port: Refined[Int, GreaterEqual[W.`1024`.T]],
    dburl: Option[Refined[String, NonEmpty]], // Even if optional, if the predicate fails for a value that exist, we should fail it and report !
    longs: Refined[List[Long], Size[Greater[W.`2`.T]]],
    pwd: Refined[String, Trimmed And NonEmpty]
  )

  def longList(n: Int): ::[ConfigDescriptor[Long]] = {
    val list =
      (1 to n).toList
        .map(group => long(s"GROUP${group}_LONGVAL"))

    ::(list.head, list.tail)
  }

  def longs(n: Int): ConfigDescriptor[List[Long]] = {
    val ll = longList(n)
    collectAll(ll.head, ll.tail: _*)
  }

  def prodConfig(n: Int): ConfigDescriptor[RefinedProd] =
    (
      nonEmpty(string("LDAP")) |@|
        greaterEqual[W.`1024`.T](int("PORT")) |@|
        nonEmpty(string("DB_URL")).optional |@|
        size[Greater[W.`2`.T]](longs(n)) |@|
        and[Trimmed, NonEmpty](string("PWD"))
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
      pwd   <- genSymbol(1, 10)
    } yield RefinedProd(
      Refined.unsafeApply(ldap),
      Refined.unsafeApply(port),
      dburl.map(Refined.unsafeApply),
      Refined.unsafeApply(::(longs.head, longs.tail)),
      Refined.unsafeApply(pwd)
    )

  def genRefinedProdInvalid: Gen[Random, (Int, Map[String, String])] =
    for {
      port  <- Gen.int(0, 1023)
      n     <- Gen.int(1, 2)
      longs <- Gen.listOfN(n)(Gen.anyLong)
    } yield (
      n,
      Map(
        "LDAP"   -> "",
        "DB_URL" -> "",
        "PORT"   -> port.toString,
        "COUNT"  -> n.toString,
        "PWD"    -> ""
      ) ++ longs
        .foldRight[List[(String, String)]](Nil)(
          (v, list) => s"GROUP${list.size + 1}_LONGVAL" -> v.toString :: list
        )
    )

}
