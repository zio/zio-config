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
import zio.config.{BaseSpec, ConfigDescriptor, ConfigSource, read, write}
import zio.test.Assertion._
import zio.test.{Gen, _}

import ConfigDescriptor._

object RefinedReadWriteRoundtripTest extends BaseSpec {

  val spec: Spec[TestConfig with Any, TestFailure[String], TestSuccess] =
    suite("Refined support")(
      test("Refined config roundtrip") {
        check(genRefinedProd) { p =>
          val cfg = prodConfig(p.longs.value.size)
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))
                  .mapError(_.getMessage)
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config invalid") {
        check(genRefinedProdInvalid) { case (n, envMap) =>
          val p2 =
            read(prodConfig(n) from ConfigSource.fromMap(envMap))

          assertM(p2.mapError(_.size).either)(equalTo(Left(5)))
        }
      }
    )
}

object RefinedReadWriteRoundtripTestUtils {

  case class RefinedProd(
    ldap: Refined[String, NonEmpty],
    port: Refined[Int, GreaterEqual[W.`1024`.T]],
    dburl: Option[
      Refined[String, NonEmpty]
    ], // Even if optional, if the predicate fails for a value that exist, we should fail it and report !
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
      refine[String, NonEmpty]("LDAP") zip
        refine[Int, GreaterEqual[W.`1024`.T]]("PORT") zip
        refine[String, NonEmpty]("DB_URL").optional zip
        refine[Size[Greater[W.`2`.T]]](longs(n)) zip
        refine[String, And[Trimmed, NonEmpty]]("PWD")
    ).to[RefinedProd]

  ////

  def genRefinedProd: Gen[Any, RefinedProd] =
    for {
      ldap  <- genSymbol(1, 10)
      port  <- Gen.int(1025, 64000)
      dburl <- Gen.option(genSymbol(1, 20))
      n     <- Gen.int(3, 10)
      longs <- Gen.listOfN(n)(Gen.long)
      pwd   <- genSymbol(1, 10)
    } yield RefinedProd(
      Refined.unsafeApply(ldap),
      Refined.unsafeApply(port),
      dburl.map(Refined.unsafeApply),
      Refined.unsafeApply(::(longs.head, longs.tail)),
      Refined.unsafeApply(pwd)
    )

  def genRefinedProdInvalid: Gen[Any, (Int, Map[String, String])] =
    for {
      port  <- Gen.int(0, 1023)
      n     <- Gen.int(1, 2)
      longs <- Gen.listOfN(n)(Gen.long)
    } yield (
      n,
      Map(
        "LDAP"   -> "",
        "DB_URL" -> "",
        "PORT"   -> port.toString,
        "COUNT"  -> n.toString,
        "PWD"    -> ""
      ) ++ longs
        .foldRight[List[(String, String)]](Nil)((v, list) => s"GROUP${list.size + 1}_LONGVAL" -> v.toString :: list)
    )

}
