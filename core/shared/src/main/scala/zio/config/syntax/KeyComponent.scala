package zio.config.syntax

import zio.Chunk
import zio.config.syntax.KeyComponent.KeyName
import scala.util.matching.Regex
import scala.util.Try

sealed trait KeyComponent {
  def value = this match {
    case KeyName(name)             => name
    case KeyComponent.Index(value) => s"[${value}]"
  }

}

object KeyComponent {
  final case class Index(index: Int) extends KeyComponent

  final case class KeyName(name: String) extends KeyComponent

  def pretty(keyComponents: Chunk[KeyComponent]): String =
    keyComponents.map(_.value).mkString

  private val pattern: Regex = """([a-zA-Z0-9 -@\-^-~]*)(\[([0-9])*\])?""".r.anchored

  private[config] def from(s: String): Chunk[KeyComponent] =
    Chunk
      .fromIterable(
        pattern
          .findAllIn(s)
          .matchData
          .filter(_.group(0).nonEmpty)
          .toList
      )
      .flatMap { regexMatched =>
        val optionalKey = Option(regexMatched.group(1))
          .flatMap(s => if (s.isEmpty) None else Some(s))

        val optionalValue = Option(regexMatched.group(3))
          .flatMap(s => if (s.isEmpty) None else Try(s.toInt).toOption)

        (optionalKey, optionalValue) match {
          case (Some(key), Some(value)) => Chunk(KeyComponent.KeyName(key), KeyComponent.Index(value))
          case (None, Some(value))      => Chunk(KeyComponent.Index(value))
          case (Some(key), None)        => Chunk(KeyComponent.KeyName(key))
          case (None, None)             => Chunk.empty
        }
      }

}
