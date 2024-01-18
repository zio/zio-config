package zio.config.typesafe

import zio.Chunk
import zio.config.IndexedFlat.KeyComponent

sealed trait HoconObject { self =>
  def flatten: Map[Chunk[KeyComponent], String] = {
    def go(hoconObject: HoconObject, path: Chunk[KeyComponent]): Map[Chunk[KeyComponent], String] =
      hoconObject match {
        case HoconObject.Text(value)    => Map(path -> value)
        case HoconObject.Record(record) =>
          record.flatMap { case (key, value) =>
            val subNewPath =
              path ++ Chunk(KeyComponent.KeyName(key))

            go(value, subNewPath)
          }

        case HoconObject.Sequence(chunk) =>
          chunk.zipWithIndex
            .map({ case (hocon, index) =>
              go(hocon, path :+ KeyComponent.Index(index))
            })
            .reduceOption(_ ++ _)
            .getOrElse(Map.empty)
      }

    go(self, Chunk.empty)
  }
}

object HoconObject {

  final case class Text(value: String)                      extends HoconObject
  final case class Record(record: Map[String, HoconObject]) extends HoconObject
  final case class Sequence(chunk: Chunk[HoconObject])      extends HoconObject
}
