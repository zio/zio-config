package zio.config.magnolia

import zio.config.PropertyTree._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config._
import zio.test.Assertion._
import zio.test._

object CoproductSealedTraitSpec extends DefaultRunnableSpec {

  sealed trait X

  case object A                extends X
  case object B                extends X
  case object C                extends X
  case class D(detail: Detail) extends X
  case class E(detail: Detail) extends X
  case class Detail(firstName: String, lastName: String, region: Region)
  case class Region(suburb: String, city: String)

  case class Config(x: X)

  val spec = suite("MagnoliaConfig")(test("descriptor of coproduct sealed trait") {
    assert(read(descriptor[Config] from ConfigSource.fromMap(Map("x" -> "a"))))(equalTo(Right(Config(A)))) &&
    assert(read(descriptor[Config] from ConfigSource.fromMap(Map("x" -> "b"))))(equalTo(Right(Config(B)))) &&
    assert(read(descriptor[Config] from ConfigSource.fromMap(Map("x" -> "c"))))(equalTo(Right(Config(C)))) &&
    assert(
      read(
        descriptor[Config] from ConfigSource.fromMap(
          constantMap = Map(
            "x.d.detail.firstName"     -> "ff",
            "x.d.detail.lastName"      -> "ll",
            "x.d.detail.region.suburb" -> "strath",
            "x.d.detail.region.city"   -> "syd"
          ),
          keyDelimiter = Some('.')
        )
      )
    )(equalTo(Right(Config(D(Detail("ff", "ll", Region("strath", "syd"))))))) &&
    assert(
      read(
        descriptor[Config] from ConfigSource.fromMap(
          Map(
            "x.e.detail.firstName"     -> "ff",
            "x.e.detail.lastName"      -> "ll",
            "x.e.detail.region.suburb" -> "strath",
            "x.e.detail.region.city"   -> "syd"
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
                  "d" -> Record(
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
    assert(write(descriptor[Config], Config(A)))(equalTo(Right(Record(Map("x" -> Leaf("a")))))) &&
    assert(write(descriptor[Config], Config(B)))(equalTo(Right(Record(Map("x" -> Leaf("b")))))) &&
    assert(write(descriptor[Config], Config(C)))(equalTo(Right(Record(Map("x" -> Leaf("c"))))))
  })
}
