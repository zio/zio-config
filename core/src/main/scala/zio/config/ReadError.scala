package zio.config

sealed trait ReadError[A] extends Exception { self =>
  override def toString: String = self match {
    case ReadError.MissingValue(path)             => s"MissingValue(${path})"
    case ReadError.FormatError(path, message)     => s"FormatError(${path}, ${message})"
    case ReadError.ConversionError(path, message) => s"ConversionError(${path},${message}"
    case ReadError.OrErrors(list)                 => s"OrErrors(${list.map(_.toString)})"
    case ReadError.AndErrors(list)                => s"AndErrors(${list.map(_.toString)}"
  }

  def size: Int =
    self match {
      case ReadError.MissingValue(_)       => 1
      case ReadError.FormatError(_, _)     => 1
      case ReadError.ConversionError(_, _) => 1
      case ReadError.OrErrors(list)        => list.map(_.size).sum
      case ReadError.AndErrors(list)       => list.map(_.size).sum
    }
}

object ReadError {
  sealed trait Step[+K]
  case class KeyStep[+K](key: K)   extends Step[K]
  case class IndexStep(index: Int) extends Step[Nothing]

  final case class MissingValue[A](path: List[Step[A]])                     extends ReadError[A]
  final case class FormatError[A](path: List[Step[A]], message: String)     extends ReadError[A]
  final case class ConversionError[A](path: List[Step[A]], message: String) extends ReadError[A]
  final case class OrErrors[A](list: List[ReadError[A]])                    extends ReadError[A]
  final case class AndErrors[A](list: List[ReadError[A]])                   extends ReadError[A]
}
