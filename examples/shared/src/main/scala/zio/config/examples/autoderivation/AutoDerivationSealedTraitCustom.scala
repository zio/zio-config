package zio.config.examples.autoderivation

import zio.config._
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.TypesafeConfigSource

import examples._
import zio.config.derivation.name

object AutoDerivationSealedTraitCustom extends App with EitherImpureOps {

  /**
   * An example that describes automatic derivation with a few name customisations (done using annotations)
   * for coproducts (sealed traits) with objects and case classes as terms
   */
  final case class AppConfig(awsConfig: AwsConfig, appName: String)
  final case class AwsConfig(field: RandomSealedTrait1)

  sealed trait RandomSealedTrait1

  object RandomSealedTrait1 {
    @name("customname1")
    case object SubObject1 extends RandomSealedTrait1
    case object SubObject2 extends RandomSealedTrait1

    final case class Trait1SubClass1(value: String, g: RandomCaseClass) extends RandomSealedTrait1

    @name("customname2")
    final case class Trait1SubClass2(value: RandomSealedTrait2)                        extends RandomSealedTrait1
    final case class Trait1SubClass3(@name("customfieldname") a: String, b: Int)       extends RandomSealedTrait1
    final case class Trait1SubClass4(a: String, b: Option[Int], c: RandomSealedTrait2) extends RandomSealedTrait1

    final case class RandomCaseClass(l: String)

  }

  sealed trait RandomSealedTrait2

  object RandomSealedTrait2 {
    case class Trait2SubClass(a: String) extends RandomSealedTrait2
  }

  import RandomSealedTrait1._
  import RandomSealedTrait2._

  val s1: String =
    """
      |awsConfig {
      | field {
      |   Trait1SubClass1 {
      |     value = b
      |     g {
      |       l = hi
      |     }
      |    }
      |  }
      |}
      |appName = l
      |
      |""".stripMargin

  assert(
    read(deriveConfig[AppConfig] from TypesafeConfigSource.fromHoconString(s1)) equalM
      AppConfig(AwsConfig(Trait1SubClass1("b", RandomCaseClass("hi"))), "l")
  )

  val s2: String =
    """
      |field = customname1
      |""".stripMargin

  assert(read(deriveConfig[AwsConfig] from TypesafeConfigSource.fromHoconString(s2)) equalM AwsConfig(SubObject1))

  val s3: String =
    """
      |field = SubObject2
      |""".stripMargin

  assert(read(deriveConfig[AwsConfig] from TypesafeConfigSource.fromHoconString(s3)) equalM AwsConfig(SubObject2))

  val s4: String =
    """
      |field {
      |  customname2 {
      |   value {
      |       Trait2SubClass {
      |         a = 1
      |       }
      |     }
      |  }
      |}
      |""".stripMargin

  assert(
    read(deriveConfig[AwsConfig] from TypesafeConfigSource.fromHoconString(s4))
      .mapError(_.prettyPrint()) equalM AwsConfig(
      Trait1SubClass2(Trait2SubClass("1"))
    )
  )

  val s5: String =
    """
      |field {
      |    Trait1SubClass3 {
      |      customfieldname = 1
      |      b = 2
      |    }
      |}
      |""".stripMargin

  assert(
    read(deriveConfig[AwsConfig] from TypesafeConfigSource.fromHoconString(s5)) equalM AwsConfig(
      Trait1SubClass3("1", 2)
    )
  )

  val s6: String =
    """
      |field {
      |    Trait1SubClass4 {
      |      a = 1
      |      c {
      |          Trait2SubClass {
      |            a = 2
      |          }
      |      }
      |    }
      |}
      |""".stripMargin

  assert(
    read(deriveConfig[AwsConfig] from TypesafeConfigSource.fromHoconString(s6)) equalM
      AwsConfig(Trait1SubClass4("1", None, Trait2SubClass("2")))
  )

  val s7: String =
    """
      |field {
      |    Trait1SubClass4 {
      |      a = 1
      |      b = 2
      |      c {
      |          Trait2SubClass {
      |            a = 2
      |          }
      |      }
      |    }
      |}
      |""".stripMargin

  assert(
    read(deriveConfig[AwsConfig] from TypesafeConfigSource.fromHoconString(s7)) equalM
      AwsConfig(Trait1SubClass4("1", Some(2), Trait2SubClass("2")))
  )

}
