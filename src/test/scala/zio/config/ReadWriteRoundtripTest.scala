package zio.config

import org.scalacheck.Properties
import zio.config.testsupport.TestSupport

object ReadWriteRoundtripTest extends Properties("Coproduct support") with TestSupport {

  val genLdap = genNonEmptyString(20).map(Ldap)
  val genDbUrl = genNonEmptyString(20).map(DbUrl)
  val genEnterpriseAuth =
    for {
      ldap <- genLdap
        dburl <- genDbUrl
    } yield EnterpriseAuth(ldap, dburl)
  val genNestedStuff =
    for {
      auth <- genEnterpriseAuth
        count <- genFor[Int]
        factor <- genFor[Double]
    } yield NestedStuff(auth, count, factor)

  val cLdap: Config[Ldap] = string("kLdap").xmap(Ldap)(_.value)
  val cDbUrl: Config[DbUrl] = string("kDbUrl").xmap(DbUrl)(_.value)
  val cEnterpriseAuth: Config[EnterpriseAuth] =
    (cLdap <*> cDbUrl) (
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
  val cNestedStuff: Config[NestedStuff] =
    (cEnterpriseAuth <*> int("kCount") <*> double("kFactor")) (
      NestedStuff.apply,
      NestedStuff.unapply
    )

  property("newtype 1 roundtrip") = forAllZIO(genLdap) { p =>
    val p2 =
      for {
        written <- write(cLdap).run.provide(p)
          reread <- read(cLdap).run.provide(mapSource(written.allConfig))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("newtype 2 roundtrip") = forAllZIO(genDbUrl) { p =>
    val p2 =
      for {
        written <- write(cDbUrl).run.provide(p)
          reread <- read(cDbUrl).run.provide(mapSource(written.allConfig))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("case class 1 roundtrip") = forAllZIO(genEnterpriseAuth) { p =>
    val p2 =
      for {
        written <- write(cEnterpriseAuth).run.provide(p)
          reread <- read(cEnterpriseAuth).run.provide(mapSource(written.allConfig))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("nested case class roundtrip") = forAllZIO(genNestedStuff) { p =>
    val p2 =
      for {
        written <- write(cNestedStuff).run.provide(p)
          reread <- read(cNestedStuff).run.provide(mapSource(written.allConfig))
      } yield reread._2

    p2.shouldBe(p)
  }

  ////

  final case class Ldap(value: String) extends AnyVal
  final case class DbUrl(value: String) extends AnyVal
  final case class EnterpriseAuth(ldap: Ldap, dburl: DbUrl)
  final case class NestedStuff(enterpriseAuth: EnterpriseAuth, count: Int, factor: Double)
}
