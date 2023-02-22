package zio.config.yaml.generators
import zio.Chunk
import zio.config.xml.XmlObject
import zio.test.Gen
import zio.config.yaml.generators.WhiteSpacedAttributes.RandomAttribute


final case class WhiteSpacedAttributes private (value: Chunk[(RandomAttribute, Space)]) {

  def print: String =
    Printer.print(value.map { case (attribute, space) => Printer.print(attribute.print, space.print) }: _*)
}

object WhiteSpacedAttributes {

  def gen(minNumberOfAttributes: Int, maxNumberOfAttributes: Int): Gen[Any, WhiteSpacedAttributes] =
    for {
      numberOfAttributes <- Gen.int(minNumberOfAttributes, maxNumberOfAttributes)
      randomAttributes   <- Gen.chunkOfN(numberOfAttributes)(RandomAttribute.gen)
      spaces             <- Gen.chunkOfN(numberOfAttributes)(Space.gen(1))
      value               = randomAttributes
                              .zip(spaces)
    } yield WhiteSpacedAttributes(value)

  final case class RandomAttribute(
    key: WhiteSpacedText,
    value: WhiteSpacedText
  ) {

    def toAttribute: XmlObject.Attribute =
      (key.value, value.value.drop(1).dropRight(1))

    def print: String = {
      val result = Printer.print(key.print, "=", value.print)

      result
    }
  }

  object RandomAttribute {

    def gen =
      for {
        start                     <- Space.gen(0)
        stop                      <- Space.gen(0)
        key                       <- Gen.chunkOfN(30)(Gen.alphaNumericChar)
        validKey                   = key.filterNot(List('<', '>', '=', ' ').contains).mkString
        spacedKey                  = WhiteSpacedText(start, validKey, stop)
        rawValue                  <- Gen.chunkOfN(30)(Gen.alphaNumericChar)
        valueWithSpaceInsideQuotes = Printer.print(
                                       "\"",
                                       Printer.print(start.print, rawValue.mkString, stop.print),
                                       "\""
                                     )
        spacedValue                = WhiteSpacedText(start, valueWithSpaceInsideQuotes, stop)
      } yield RandomAttribute(spacedKey, spacedValue)
  }
}
