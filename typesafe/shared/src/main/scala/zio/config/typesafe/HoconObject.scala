package zio.config.typesafe

import zio.Chunk
import zio.config.IndexedFlat.KeyComponent

sealed trait HoconObject { self =>
  def flatten: Map[Chunk[KeyComponent], String] = {
    def go(hoconObject: HoconObject, path: Chunk[KeyComponent]): Map[Chunk[KeyComponent], String] =
      hoconObject match {
        case HoconObject.Text(value)          => Map(path -> value)
        case HoconObject.KeyValue(key, value) =>
          val parentNewPath = path ++ Chunk(KeyComponent.KeyName(key))
          go(value, parentNewPath)

        case HoconObject.Sequence(chunk) =>
          chunk
            .map(go(_, path))
            .reduceOption(_ ++ _)
            .getOrElse(Map.empty)
      }

    go(self, Chunk.empty)
  }
}

object HoconObject {

  final case class Text(value: String)                       extends HoconObject
  final case class KeyValue(key: String, value: HoconObject) extends HoconObject
  final case class Sequence(chunk: Chunk[HoconObject])       extends HoconObject
}
