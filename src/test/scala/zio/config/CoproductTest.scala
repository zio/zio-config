package zio.config.examples

import org.scalacheck.Prop.forAll
import org.scalacheck.{ Gen, Properties }
import zio.DefaultRuntime
import zio.config.ConfigError.MissingValue
import zio.config.{ Config, ConfigError, ConfigSource, _ }

object CoproductTest extends Properties("Coproduct") {

  def genString(minLength: Int, maxLength: Int): Gen[String] =
    for {
      n <- Gen.chooseNum(minLength, maxLength)
      s <- Gen.listOfN(n, Gen.alphaChar).map(_.mkString)
    } yield s

  ////

  final case class CoproductParams(x1: String, x2: String, x3: String, x4: String, x5: String)

  object CoproductParams {
    def gen =
      for {
        x1 <- genString(1, 20)
        x2 <- genString(1, 20).filterNot(s => s == x1)
        x3 <- genString(1, 20).filterNot(s => s == x1 || s == x2)
        x4 <- genString(1, 20).filterNot(s => s == x1 || s == x2 || s == x3)
        x5 <- genString(1, 20).filterNot(s => s == x1 || s == x2 || s == x3 || s == x4)
      } yield CoproductParams(x1, x2, x3, x4, x5)
  }

  //propertyWithSeed("Test1", Some("zuURCIhT4Fs1NirKL275VtwGk_Vez9iAo7riHP0fqwG=")) =
  property("left") = forAll(CoproductParams.gen) {
    testLeft(_)
  }

  property("right") = forAll(CoproductParams.gen) {
    test2(_)
  }

  property("errors") = forAll(CoproductParams.gen) {
    test3(_)
  }

  property("allConfigsExist") = forAll(CoproductParams.gen) {
    test4(_)
  }

  ////

  final case class Ldap(value: String)  extends AnyVal
  final case class DbUrl(value: String) extends AnyVal
  final case class Prod(ldap: Ldap, dburl: DbUrl)
  final case class Dev(user: String, password: Int, dburl: Double)

  val rt = new DefaultRuntime {}

  def testLeft(p: CoproductParams): Boolean = {
    val prod: Config[Prod]                   = (string(p.x1).map(Ldap) <*> string(p.x2).map(DbUrl))(Prod.apply, Prod.unapply)
    val dev: Config[Dev]                     = (string(p.x3) <*> int(p.x4) <*> double(p.x5))(Dev.apply, Dev.unapply)
    val prodOrDev: Config[Either[Prod, Dev]] = prod or dev

    val validConfigForSampleConfig =
      Map(
        p.x1 -> "v1",
        p.x2 -> "v2",
        p.x3 -> "v3"
      )

    val source: ConfigSource = mapSource(validConfigForSampleConfig)

    rt.unsafeRun(read(prodOrDev).run.provide(source))._2 == Left(Prod(Ldap("v1"), DbUrl("v2")))
  }

  def test2(p: CoproductParams): Boolean = {
    val prod: Config[Prod]                   = (string(p.x1).map(Ldap) <*> string(p.x2).map(DbUrl))(Prod.apply, Prod.unapply)
    val dev: Config[Dev]                     = (string(p.x3) <*> int(p.x4) <*> double(p.x5))(Dev.apply, Dev.unapply)
    val prodOrDev: Config[Either[Prod, Dev]] = prod or dev

    val validConfigForAnotherConfig =
      Map(
        p.x2 -> "v2",
        p.x3 -> "v3",
        p.x4 -> "1",
        p.x5 -> "2.0"
      )

    val anotherSource: ConfigSource = mapSource(validConfigForAnotherConfig)

    rt.unsafeRun(read(prodOrDev).run.provide(anotherSource))._2 == Right(Dev("v3", 1, 2.0))
  }

  def test3(p: CoproductParams): Boolean = {
    val prod: Config[Prod]                   = (string(p.x1).map(Ldap) <*> string(p.x2).map(DbUrl))(Prod.apply, Prod.unapply)
    val dev: Config[Dev]                     = (string(p.x3) <*> int(p.x4) <*> double(p.x5))(Dev.apply, Dev.unapply)
    val prodOrDev: Config[Either[Prod, Dev]] = prod or dev

    val invalidConfig =
      Map(
        p.x2 -> "v2",
        p.x3 -> "v3",
        p.x4 -> "1",
        p.x5 -> "notadouble"
      )

    val invalidSource: ConfigSource = mapSource(invalidConfig)

    rt.unsafeRun(read(prodOrDev).run.provide(invalidSource).either) ==
      Left(
        List(
          ConfigError(Seq(p.x1), MissingValue),
          ConfigError(Seq(p.x5), ConfigError.ParseError("notadouble", "double"))
        )
      )
  }

  def test4(p: CoproductParams): Boolean = {
    val prod: Config[Prod]                   = (string(p.x1).map(Ldap) <*> string(p.x2).map(DbUrl))(Prod.apply, Prod.unapply)
    val dev: Config[Dev]                     = (string(p.x3) <*> int(p.x4) <*> double(p.x5))(Dev.apply, Dev.unapply)
    val prodOrDev: Config[Either[Prod, Dev]] = prod or dev

    val allConfigsExist =
      Map(
        p.x1 -> "v1",
        p.x2 -> "v2",
        p.x3 -> "v3",
        p.x4 -> "1",
        p.x5 -> "2.0"
      )

    rt.unsafeRun(read(prodOrDev).run.provide(mapSource(allConfigsExist)))._2 == Left(Prod(Ldap("v1"), DbUrl("v2")))
  }
}
