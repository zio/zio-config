package zio.config.examples.magnolia

import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor._
import zio.config.ConfigSource
import zio.config.PropertyTree._
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.magnolia.DeriveConfigDescriptor

object CoproductSealedTraitExample extends App with EitherImpureOps {

  sealed trait X

  case object A                extends X
  case object B                extends X
  case object C                extends X
  case class D(detail: Detail) extends X
  case class E(detail: Detail) extends X
  case class Detail(firstName: String, lastName: String, region: Region)
  case class Region(suburb: String, city: String)

  println(read(DeriveConfigDescriptor[X] from ConfigSource.fromMap(Map("x" -> "b"))))

  assert(read(DeriveConfigDescriptor[X] from ConfigSource.fromMap(Map("x" -> "a"))) == Right(A))
  assert(read(DeriveConfigDescriptor[X] from ConfigSource.fromMap(Map("x" -> "b"))) == Right(B))
  assert(read(DeriveConfigDescriptor[X] from ConfigSource.fromMap(Map("x" -> "c"))) == Right(C))
  assert(
    read(
      descriptor[X] from ConfigSource.fromMap(
        map = Map(
          "x.d.detail.firstName"     -> "ff",
          "x.d.detail.lastName"      -> "ll",
          "x.d.detail.region.suburb" -> "strath",
          "x.d.detail.region.city"   -> "syd"
        ),
        keyDelimiter = Some('.')
      )
    ) == Right(
      D(Detail("ff", "ll", Region("strath", "syd")))
    )
  )
  assert(
    read(
      descriptor[X] from ConfigSource.fromMap(
        Map(
          "x.e.detail.firstName"     -> "ff",
          "x.e.detail.lastName"      -> "ll",
          "x.e.detail.region.suburb" -> "strath",
          "x.e.detail.region.city"   -> "syd"
        ),
        keyDelimiter = Some('.')
      )
    ) == Right(
      E(Detail("ff", "ll", Region("strath", "syd")))
    )
  )

  assert(
    write(DeriveConfigDescriptor[X], D(Detail("ff", "ll", Region("strath", "syd")))) ==
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

  assert(write(DeriveConfigDescriptor[X], A) == Right(Record(Map("x" -> Leaf("a")))))
  assert(write(DeriveConfigDescriptor[X], B) == Right(Record(Map("x" -> Leaf("b")))))
  assert(write(DeriveConfigDescriptor[X], C) == Right(Record(Map("x" -> Leaf("c")))))
}
