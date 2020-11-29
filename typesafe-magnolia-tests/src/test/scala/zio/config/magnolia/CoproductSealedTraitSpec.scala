package zio.config.magnolia

import zio.config.PropertyTree._
import zio.config._
import zio.test.Assertion._
import zio.test._

object CoproductSealedTraitSpec extends DefaultRunnableSpec {

  sealed trait X

  case object A extends X
  case object B extends X
  @name("c")
  case object C                extends X
  case class D(detail: Detail) extends X
  case class E(detail: Detail) extends X
  case class Detail(firstName: String, lastName: String, region: Region)
  case class Region(suburb: String, city: String)

  case class Config(x: X)

  val spec = suite("MagnoliaConfig")(test("descriptor of coproduct sealed trait") {
    assert(read(descriptor[Config] from ConfigSource.fromMap(Map("x" -> "A"))))(equalTo(Right(Config(A)))) &&
    assert(read(descriptor[Config] from ConfigSource.fromMap(Map("x" -> "B"))))(equalTo(Right(Config(B)))) &&
    assert(read(descriptor[Config] from ConfigSource.fromMap(Map("x" -> "c"))))(equalTo(Right(Config(C)))) &&
    assert(
      read(
        descriptor[Config] from ConfigSource.fromMap(
          constantMap = Map(
            "x.D.detail.firstName"     -> "ff",
            "x.D.detail.lastName"      -> "ll",
            "x.D.detail.region.suburb" -> "strath",
            "x.D.detail.region.city"   -> "syd"
          ),
          keyDelimiter = Some('.')
        )
      )
    )(equalTo(Right(Config(D(Detail("ff", "ll", Region("strath", "syd"))))))) &&
    assert(
      read(
        descriptor[Config] from ConfigSource.fromMap(
          Map(
            "x.E.detail.firstName"     -> "ff",
            "x.E.detail.lastName"      -> "ll",
            "x.E.detail.region.suburb" -> "strath",
            "x.E.detail.region.city"   -> "syd"
          ),
          keyDelimiter = Some('.')
        )
      )
    )(equalTo(Right(Config(E(Detail("ff", "ll", Region("strath", "syd"))))))) &&
    assert(write(descriptor[Config], Config(D(Detail("ff", "ll", Region("strath", "syd"))))))(
      equalTo(
        Right(
          Record(
            Map(
              "x" -> Record(
                Map(
                  "D" -> Record(
                    Map(
                      "detail" -> Record(
                        Map(
                          "region"    -> Record(Map("city" -> Leaf("syd"), "suburb" -> Leaf("strath"))),
                          "lastName"  -> Leaf("ll"),
                          "firstName" -> Leaf("ff")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ) &&
    assert(write(descriptor[Config], Config(A)))(equalTo(Right(Record(Map("x" -> Leaf("A")))))) &&
    assert(write(descriptor[Config], Config(B)))(equalTo(Right(Record(Map("x" -> Leaf("B")))))) &&
    assert(write(descriptor[Config], Config(C)))(equalTo(Right(Record(Map("x" -> Leaf("c"))))))
  })
}
