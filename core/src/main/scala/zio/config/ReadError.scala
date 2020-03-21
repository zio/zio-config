package zio.config

sealed trait ReadError[A] { self =>
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
}

object ReadError {
  final case class MissingValue[A](path: Vector[Either[Int, A]])                     extends ReadError[A]
  final case class FormatError[A](path: Vector[Either[Int, A]], message: String)     extends ReadError[A]
  final case class ConversionError[A](path: Vector[Either[Int, A]], message: String) extends ReadError[A]
  final case class OrErrors[A](list: List[ReadError[A]])                             extends ReadError[A]
  final case class AndErrors[A](list: List[ReadError[A]])                            extends ReadError[A]
}
