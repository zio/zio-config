package zio.config

import org.scalacheck.Properties
import zio.ZIO
import zio.config.testsupport.TestSupport

object ReadWriteRoundtripTest extends Properties("Coproduct support") with TestSupport {

  val genLdap = genNonEmptyString(20).map(Ldap)
  val genDbUrl = genNonEmptyString(20).map(DbUrl)
  val genEnterpriseAuth =
    for {
      ldap <- genLdap
        dburl <- genDbUrl
    } yield EnterpriseAuth(ldap, dburl)
  val genPasswordAuth =
    for {
      user <- genNonEmptyString(30)
        count <- genFor[Int]
        factor <- genFor[Double]
    } yield PasswordAuth(user, count, factor)

  // TODO don't think map works
  val cLdap: Config[Ldap] = string("kLdap").map(Ldap)
  val cDbUrl: Config[DbUrl] = string("kDbUrl").map(DbUrl)
  val enterprise: Config[EnterpriseAuth] =
    (cLdap <*> cDbUrl) (
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
  val password: Config[PasswordAuth] =
    (string("kUser") <*> int("kCount") <*> double("kFactor")) (
      PasswordAuth.apply,
      PasswordAuth.unapply
    )

  property("left element satisfied") =
    forAllZIO(genLdap) {
      p =>
        val p2: ZIO[Any, List[ConfigError], Ldap] =
          for {
              written <- write(cLdap).run.provide(p)
              reread <- read(cLdap).run.provide(mapSource(written.allConfig))
          } yield reread._2

        p2.shouldBe(p)
    }

  ////

  final case class Ldap(value: String) extends AnyVal
  final case class DbUrl(value: String) extends AnyVal
  final case class EnterpriseAuth(ldap: Ldap, dburl: DbUrl)
  final case class PasswordAuth(user: String, count: Int, factor: Double)
}
