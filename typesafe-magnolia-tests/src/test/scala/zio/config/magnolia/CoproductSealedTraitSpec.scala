package zio.config.magnolia

import zio.ConfigProvider
import zio.config._
import zio.config.derivation.{name => derivedName}
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

object CoproductSealedTraitSpec extends ZIOSpecDefault {

  sealed trait X

  case object A                extends X
  case object B                extends X
  @derivedName("c")
  case object C                extends X
  case class D(detail: Detail) extends X
  case class E(detail: Detail) extends X
  case class Detail(firstName: String, lastName: String, region: Region)
  case class Region(suburb: String, city: String)

  case class Config(x: X)

  def spec: Spec[Any, Any] =
    suite("MagnoliaConfig")(test("deriveConfig of coproduct sealed trait") {
      assertZIO(read(deriveConfig[Config] from ConfigProvider.fromMap(Map("x" -> "A"))))(equalTo(Config(A))) *>
        assertZIO(read(deriveConfig[Config] from ConfigProvider.fromMap(Map("x" -> "B"))))(equalTo(Config(B))) *>
        assertZIO(read(deriveConfig[Config] from ConfigProvider.fromMap(Map("x" -> "c"))))(equalTo(Config(C))) *>
        assertZIO(
          read(
            deriveConfig[Config] from ConfigProvider.fromMap(
              Map(
                "x.D.detail.firstName"     -> "ff",
                "x.D.detail.lastName"      -> "ll",
                "x.D.detail.region.suburb" -> "strath",
                "x.D.detail.region.city"   -> "syd"
              )
            )
          )
        )(equalTo(Config(D(Detail("ff", "ll", Region("strath", "syd")))))) *>
        assertZIO(
          read(
            deriveConfig[Config] from ConfigProvider.fromMap(
              Map(
                "x.E.detail.firstName"     -> "ff",
                "x.E.detail.lastName"      -> "ll",
                "x.E.detail.region.suburb" -> "strath",
                "x.E.detail.region.city"   -> "syd"
              )
            )
          )
        )(equalTo(Config(E(Detail("ff", "ll", Region("strath", "syd"))))))
    })
}
