package zio.config

sealed trait ReadError[A] extends Exception { self =>
  override def toString: String = self match {
    case ReadError.MissingValue(path)             => s"MissingValue(${path})"
    case ReadError.FormatError(path, message)     => s"FormatError(${path}, ${message})"
    case ReadError.ConversionError(path, message) => s"ConversionError(${path},${message}"
    case ReadError.OrErrors(list)                 => s"OrErrors(${list.map(_.toString)})"
    case ReadError.AndErrors(list)                => s"AndErrors(${list.map(_.toString)}"
  }

  def atKey(key: A): ReadError[A] =
    self match {
      case ReadError.MissingValue(path)             => ReadError.MissingValue(path :+ Right(key))
      case ReadError.FormatError(path, message)     => ReadError.FormatError(path :+ Right(key), message)
      case ReadError.ConversionError(path, message) => ReadError.ConversionError(path :+ Right(key), message)
      case ReadError.OrErrors(list)                 => ReadError.OrErrors(list.map(_.atKey(key)))
      case ReadError.AndErrors(list)                => ReadError.AndErrors(list.map(_.atKey(key)))
    }
  def atIndex(index: Int): ReadError[A] =
    self match {
      case ReadError.MissingValue(path)             => ReadError.MissingValue(path :+ Left(index))
      case ReadError.FormatError(path, message)     => ReadError.FormatError(path :+ Left(index), message)
      case ReadError.ConversionError(path, message) => ReadError.ConversionError(path :+ Left(index), message)
      case ReadError.OrErrors(list)                 => ReadError.OrErrors(list.map(_.atIndex(index)))
      case ReadError.AndErrors(list)                => ReadError.AndErrors(list.map(_.atIndex(index)))
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
  final case class MissingValue[A](path: Vector[Either[Int, A]])                     extends ReadError[A]
  final case class FormatError[A](path: Vector[Either[Int, A]], message: String)     extends ReadError[A]
  final case class ConversionError[A](path: Vector[Either[Int, A]], message: String) extends ReadError[A]
  final case class OrErrors[A](list: List[ReadError[A]])                             extends ReadError[A]
  final case class AndErrors[A](list: List[ReadError[A]])                            extends ReadError[A]
}
