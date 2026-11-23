package zio.config.xml.experimental

import zio.Chunk
import zio.config.IndexedFlat.KeyComponent

// To be moved to zio-config
/**
 * A node of a parsed XML document, as the experimental XML parser produces it: an element (`XmlObject.TagElement`) or
 * the text inside one (`XmlObject.Text`).
 */
sealed trait XmlObject {

  /**
   * Returns every value in this tree, keyed by its path of element and attribute names. An attribute's path is its
   * element's path followed by the attribute name, and a text node's path is the path of the element that holds it.
   * When two values have the same path, a child element's value replaces an attribute's, and a later sibling's value
   * replaces an earlier one's.
   */
  def flattened: Map[Chunk[KeyComponent], String] = {
    def go(
      xmlObject: XmlObject,
      path: Chunk[KeyComponent]
    ): Map[Chunk[KeyComponent], String] =
      xmlObject match {
        case XmlObject.Text(value)                            => Map(path -> value)
        case XmlObject.TagElement(name, attributes, children) =>
          val parentNewPath = path ++ Chunk(KeyComponent.KeyName(name))

          val attributesMap: Map[Chunk[KeyComponent], String] =
            attributes.map { case (key, value) =>
              val subNewPath =
                parentNewPath ++ Chunk(KeyComponent.KeyName(key))
              subNewPath -> value
            }.toMap

          val childrenMap = children
            .map(go(_, parentNewPath))
            .reduceOption(_ ++ _)
            .getOrElse(Map.empty)

          attributesMap ++ childrenMap
      }

    go(this, Chunk.empty)

  }
}

/**
 * The two kinds of [[XmlObject]] node, and the `XmlObject.Attribute` type.
 */
object XmlObject {

  /**
   * An attribute as a pair of its name and its value.
   */
  type Attribute = (String, String)

  /**
   * The text inside an element. The parser stores it trimmed of leading and trailing whitespace.
   */
  final case class Text(value: String) extends XmlObject

  /**
   * An element with its tag name, its attributes in the order they are written, and its child elements or its text.
   */
  final case class TagElement(
    name: String,
    attributes: Chunk[Attribute],
    children: Chunk[XmlObject]
  ) extends XmlObject
}
