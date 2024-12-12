package zio.config
package magnolia

import zio.test.Assertion._
import zio.test._
import zio.ConfigProvider

object DeriveConfigSpec extends ZIOSpecDefault {

  import DeriveConfigSpecUtils._

  def spec =
    suiteAll("DeriveConfig") {
      suite(".derived")(
        test("derive config for case class") {

          val desc   = deriveConfig[Test]
          val source =
            ConfigProvider.fromMap(Map("aaBbCc" -> "1"))

          assertZIO(source.load(desc))(equalTo(Test(1)))
        },
        test("derive config for case class with default value") {

          val desc   = deriveConfig[TestDefaultValue]
          val source =
            ConfigProvider.fromMap(Map.empty)

          assertZIO(source.load(desc))(equalTo(TestDefaultValue(1)))
        },
        test("derive config for case class with field name annotation") {

          val desc   = deriveConfig[TestFieldName]
          val source =
            ConfigProvider.fromMap(Map("a" -> "1"))

          assertZIO(source.load(desc))(equalTo(TestFieldName(1)))
        },
        test("derive config for sealed trait with discriminator and name annotation") {

          val desc    = deriveConfig[TestDiscriminator]
          val sourceA =
            ConfigProvider.fromMap(Map("type" -> "a", "a" -> "1"))
          val sourceB =
            ConfigProvider.fromMap(Map("type" -> "TestDiscriminatorB", "b" -> "test"))

          assertZIO(sourceA.load(desc))(equalTo(TestDiscriminatorA(1))) &&
          assertZIO(sourceB.load(desc))(equalTo(TestDiscriminatorB("test")))
        },
        test("derive config for case class with snake case annotation") {

          val desc   = deriveConfig[TestSnakeCase]
          val source =
            ConfigProvider.fromMap(Map("aa_bb_cc" -> "1"))

          assertZIO(source.load(desc))(equalTo(TestSnakeCase(1)))
        },
        test("derive config for case class with kebab case annotation") {

          val desc   = deriveConfig[TestKebabCase]
          val source =
            ConfigProvider.fromMap(Map("aa-bb-cc" -> "1"))

          assertZIO(source.load(desc))(equalTo(TestKebabCase(1)))
        },
        test("derive config for case class with nested config") {

          val desc   = deriveConfig[TestSnakeCaseWithNestedConfig]
          val source =
            ConfigProvider.fromMap(Map("aa_bb_cc" -> "1", "test_nested.aaBbCc" -> "2"))

          assertZIO(source.load(desc))(equalTo(TestSnakeCaseWithNestedConfig(1, Test(2))))
        },
        test("derive config for case class with nested kebab config") {

          val desc   = deriveConfig[TestSnakeCaseWithNestedKebabConfig]
          val source =
            ConfigProvider.fromMap(Map("aa_bb_cc" -> "1", "test_nested.aa-bb-cc" -> "2"))

          assertZIO(source.load(desc))(equalTo(TestSnakeCaseWithNestedKebabConfig(1, TestKebabCase(2))))
        },
        test("derive config for case class with prefix annotation") {

          val desc   = deriveConfig[TestPrefix]
          val source =
            ConfigProvider.fromMap(Map("testAaBbCc" -> "1"))

          assertZIO(source.load(desc))(equalTo(TestPrefix(1)))
        },
        test("derive config for case class with prefix and snake case annotation") {

          val desc   = deriveConfig[TestPrefixSnakeCase]
          val source =
            ConfigProvider.fromMap(Map("test_aa_bb_cc" -> "1"))

          assertZIO(source.load(desc))(equalTo(TestPrefixSnakeCase(1)))
        },
        test("derive config for case class with postfix annotation") {

          val desc   = deriveConfig[TestPostfix]
          val source =
            ConfigProvider.fromMap(Map("aaBbCcTest" -> "1"))

          assertZIO(source.load(desc))(equalTo(TestPostfix(1)))
        },
        test("derive config for case class with suffix annotation") {

          val desc   = deriveConfig[TestSuffix]
          val source =
            ConfigProvider.fromMap(Map("aaBbCcTest" -> "1"))

          assertZIO(source.load(desc))(equalTo(TestSuffix(1)))
        }
      )
    }

}

object DeriveConfigSpecUtils {

  case class Test(aaBbCc: Int)

  case class TestDefaultValue(aaBbCc: Int = 1)

  case class TestFieldName(@name("a") aaBbCc: Int)

  @discriminator("type")
  sealed trait TestDiscriminator
  @name("a")
  case class TestDiscriminatorA(a: Int)    extends TestDiscriminator
  case class TestDiscriminatorB(b: String) extends TestDiscriminator

  @snakeCase
  case class TestSnakeCase(aaBbCc: Int)

  @kebabCase
  case class TestKebabCase(aaBbCc: Int)

  @snakeCase
  case class TestSnakeCaseWithNestedConfig(aaBbCc: Int, testNested: Test)

  @snakeCase
  case class TestSnakeCaseWithNestedKebabConfig(aaBbCc: Int, testNested: TestKebabCase)

  @prefix("test")
  case class TestPrefix(aaBbCc: Int)

  @prefix("test")
  @snakeCase
  @kebabCase
  case class TestPrefixSnakeCase(aaBbCc: Int)

  @postfix("test")
  case class TestPostfix(aaBbCc: Int)

  @suffix("test")
  case class TestSuffix(aaBbCc: Int)

}
