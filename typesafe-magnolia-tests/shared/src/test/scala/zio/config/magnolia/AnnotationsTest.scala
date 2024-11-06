package zio.config.magnolia

import zio.config.read
import zio.config.typesafe.TypesafeConfigProvider
import zio.test.Assertion.equalTo
import zio.test.{assertZIO, Spec, ZIOSpecDefault}
import zio.{Config, IO}

object AnnotationsTest extends ZIOSpecDefault {

  object KebabTest {
    @kebabCase
    case class Foo(fooFoo: String)
    @kebabCase
    case class AnotherFoo(nestedAnotherFoo: String)
    @kebabCase
    case class Bar(@name("bArBaR-Bar") barBarBar: String)
    @kebabCase
    case class MyConfig(foo: Foo, anotherFoo: AnotherFoo, bar: Bar)

    val myConfigAutomatic: Config[MyConfig] = deriveConfig[MyConfig]
  }

  object SnakeTest {
    @snakeCase
    case class Foo(fooFoo: String)
    @snakeCase
    case class AnotherFoo(nestedAnotherFoo: String)
    @snakeCase
    case class Bar(@name("bArBaR-Bar") barBarBar: String)
    @snakeCase
    case class MyConfig(foo: Foo, anotherFoo: AnotherFoo, bar: Bar)

    val myConfigAutomatic: Config[MyConfig] = deriveConfig[MyConfig]
  }

  object PrefixAndPostfix {
    @postfix("test")
    case class Foo(fooFoo: String)
    @prefix("dev")
    case class AnotherFoo(nestedAnotherFoo: String)
    @snakeCase
    case class Bar(@name("bArBaR-Bar") barBarBar: String)
    @snakeCase
    @prefix("prod")
    case class AnotherBar(bar: String)
    @kebabCase
    @prefix("test")
    @postfix("deprecated")
    case class NextBar(barValue: String)
    @snakeCase
    case class MyConfig(foo: Foo, anotherFoo: AnotherFoo, bar: Bar, anotherBar: AnotherBar, nextBar: NextBar)

    val myConfigAutomatic: Config[MyConfig] = deriveConfig[MyConfig]
  }

  override def spec: Spec[Any, Config.Error] =
    suite("AnnotationsTest")(
      test("kebab case") {
        import KebabTest._
        val hocconConfig                       =
          s"""
             |foo {
             |  foo-foo = "value1"
             |}
             |another-foo {
             |  nested-another-foo = "value2"
             |}
             |bar {
             |  bArBaR-Bar = "value3"
             |}
             |""".stripMargin
        val result: IO[Config.Error, MyConfig] =
          read(myConfigAutomatic from TypesafeConfigProvider.fromHoconString(hocconConfig))
        val expected                           = MyConfig(Foo("value1"), AnotherFoo("value2"), Bar("value3"))
        assertZIO(result)(equalTo(expected))
      },
      test("snake case") {
        import SnakeTest._

        val hocconConfig                       =
          s"""
             |foo {
             |  foo_foo = "value1"
             |}
             |another_foo {
             |  nested_another_foo = "value2"
             |}
             |bar {
             |  bArBaR-Bar = "value3"
             |}
             |""".stripMargin
        val result: IO[Config.Error, MyConfig] =
          read(myConfigAutomatic from TypesafeConfigProvider.fromHoconString(hocconConfig))
        val expected                           = MyConfig(Foo("value1"), AnotherFoo("value2"), Bar("value3"))
        assertZIO(result)(equalTo(expected))
      },
      test("prefix and postfix") {
        import PrefixAndPostfix._

        val hocconConfig                       =
          s"""
             |foo {
             |  fooFooTest = "value1"
             |}
             |another_foo {
             |  devNestedAnotherFoo = "value2"
             |}
             |bar {
             |  bArBaR-Bar = "value3"
             |}
             |another_bar {
             |  prod_bar = "value4"
             |}
             |next_bar {
             |  test-bar-value-deprecated = "value5"
             |}
             |""".stripMargin
        val result: IO[Config.Error, MyConfig] =
          read(myConfigAutomatic from TypesafeConfigProvider.fromHoconString(hocconConfig))
        val expected                           =
          MyConfig(Foo("value1"), AnotherFoo("value2"), Bar("value3"), AnotherBar("value4"), NextBar("value5"))
        assertZIO(result)(equalTo(expected))
      }
    )

}
