package zio.config.syntax

import zio.Chunk
import scala.collection.immutable
import zio.config.syntax.KeyComponent.KeyName

sealed trait KeyComponent {
  def value = this match {
    case KeyName(name)             => name
    case KeyComponent.Index(value) => s"[${value}]"
  }

}

object KeyComponent {
  final case class Index(index: Int)     extends KeyComponent
  final case class KeyName(name: String) extends KeyComponent

  def pretty(keyComponents: Chunk[KeyComponent]): String = {
    def loop(keyComponents: List[KeyComponent]): List[String] =
      keyComponents match {
        case head0 :: head1 :: next =>
          (head0, head1) match {
            case (KeyComponent.KeyName(name), KeyComponent.Index(index)) => s"${name}[${index}]" :: loop(next)
            case (a, b)                                                  => s"${a.value}.${b.value}" :: loop(next)

          }
        case head :: next           => head.value :: loop(next)
        case Nil                    => Nil
      }

    loop(keyComponents.toList).mkString(".")
  }

}
