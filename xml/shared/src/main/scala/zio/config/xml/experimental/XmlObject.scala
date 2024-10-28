package zio.config.xml.experimental

import zio.Chunk
import zio.config.IndexedFlat.KeyComponent

// To be moved to zio-config
sealed trait XmlObject {

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

object XmlObject {
  type Attribute = (String, String)

  final case class Text(value: String) extends XmlObject

  final case class TagElement(
    name: String,
    attributes: Chunk[Attribute],
    children: Chunk[XmlObject]
  ) extends XmlObject
}
