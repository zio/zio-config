package zio.config.examples

import zio.config._
import zio.{Config, ConfigProvider}

// Refer AutoDerivationSealedTrait.scala for auto derivation for ADTs
object SealedTraitManualDerivation extends App {
  sealed trait FooBar

  case class Foo(a: Int) extends FooBar

  case class Bar(b: String) extends FooBar

  val config =
    Config.enumeration[FooBar](Config.int("a").to[Foo], Config.string("b").to[Bar])

  private val source =
    ConfigProvider.fromMap(
      Map(
        "a" -> "1"
      )
    )

  //TODO; Not working for option
  assert(source.load(config) equalM Foo(1))

}
