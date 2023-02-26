package zio.config.yaml

import zio.Scope
import zio.config.xml.experimental.XmlParser
import zio.config.yaml.generators.{Space, WhiteSpacedXml}
import zio.test.Assertion._
import zio.test._

object XmlParserSpec extends ZIOSpecDefault {

  def spec: Spec[Environment with TestEnvironment with Scope, Any] =
    suite("Xml parser spec")(
      test("test xml with zero children and with zero attributes") {
        check(
          WhiteSpacedXml.gen(0, 0).map(_.emptyChildren).noShrink,
          Space.gen.noShrink
        ) { (simpleXml, space) =>
          val config =
            simpleXml.printWith(space)

          val parsed = XmlParser.parse(config)

          assert(parsed)(equalTo(Right(simpleXml.toXmlObject)))
        }
      },
      test("test xml with attributes with no children") {
        check(
          WhiteSpacedXml.gen(1, 10).map(_.emptyChildren).noShrink,
          Space.gen.noShrink
        ) { (xmlWithAttributes, space) =>
          val config =
            xmlWithAttributes.printWith(space)

          val parsed = XmlParser.parse(config)

          assert(parsed)(equalTo(Right(xmlWithAttributes.toXmlObject)))
        }
      },
      // The round trip test that test any XML!
      test("test any xml with or without attributes, with or without children") {
        check(
          WhiteSpacedXml.gen(1, 10).noShrink,
          Space.gen.noShrink
        ) { (anyXml, space) =>
          val config =
            anyXml.printWith(space)

          val parsed   = XmlParser.parse(config)
          val expected = anyXml.toXmlObject

          assert(parsed)(equalTo(Right(expected)))
        }
      }
    )
}
