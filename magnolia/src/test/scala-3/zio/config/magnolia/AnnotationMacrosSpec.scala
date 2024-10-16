package zio.config
package magnolia

import zio.config.magnolia.AnnotationMacros
import zio.ConfigProvider
import zio.test.Assertion.*
import zio.test.*

object AnnotationMacrosSpec extends ZIOSpecDefault:

  def spec =
    suiteAll("AnnotationMacros") {
      suite(".nameOf")(
        test("filter all name annotations") {

          @name("myname")
          @name("myothername")
          case class WithName()

          assert(AnnotationMacros.nameOf[WithName])(equalTo(List(name("myname"), name("myothername"))))
        }
      )
      suite(".discriminatorOf")(
        test("filter all discriminator annotations") {

          @discriminator("mytype")
          @discriminator("myothertype")
          sealed trait WithDiscriminator

          assert(AnnotationMacros.discriminatorOf[WithDiscriminator])(
            equalTo(List(discriminator("mytype"), discriminator("myothertype")))
          )
        }
      )
      suite(".descriptionOf")(
        test("filter all description annotations") {

          @describe("mydescription")
          @describe("myotherdescription")
          case class WithDescription()

          assert(AnnotationMacros.descriptionOf[WithDescription])(
            equalTo(List(describe("mydescription"), describe("myotherdescription")))
          )
        }
      )
      suite(".kebabCaseOf")(
        test("filter all kebabCase annotations") {

          @kebabCase
          case class WithKebabCase()

          assert(AnnotationMacros.kebabCaseOf[WithKebabCase])(equalTo(List(kebabCase())))
        }
      )
      suite(".snakeCaseOf")(
        test("filter all snakeCase annotations") {

          @snakeCase
          case class WithSnakeCase()

          assert(AnnotationMacros.snakeCaseOf[WithSnakeCase])(equalTo(List(snakeCase())))
        }
      )
      suite(".prefixOf")(
        test("filter all prefix annotations") {

          @prefix("myprefix")
          case class WithPrefix()

          assert(AnnotationMacros.prefixOf[WithPrefix])(equalTo(List(prefix("myprefix"))))
        }
      )
      suite(".postfixOf")(
        test("filter all postfix annotations") {

          @postfix("mypostfix")
          case class WithPostfix()

          assert(AnnotationMacros.postfixOf[WithPostfix])(equalTo(List(postfix("mypostfix"))))
        }
      )
      suite(".suffixOf")(
        test("filter all suffix annotations") {

          @suffix("mysuffix")
          case class WithSuffix()

          assert(AnnotationMacros.suffixOf[WithSuffix])(equalTo(List(suffix("mysuffix"))))
        }
      )
      suite(".fieldNamesOf")(
        test("filter all field name annotations") {

          case class WithFieldName(@name("myname") name: String)

          assert(AnnotationMacros.fieldNamesOf[WithFieldName])(equalTo(List(("name", List(name("myname"))))))
        }
      )
    }

end AnnotationMacrosSpec
