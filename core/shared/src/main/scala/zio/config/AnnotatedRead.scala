package zio.config

private[config] final case class AnnotatedRead[+A](value: A, annotations: Set[AnnotatedRead.Annotation]) { self =>
  def map[B](f: A => B): AnnotatedRead[B] =
    AnnotatedRead(f(value), annotations)

  def mapEither[E, B](f: A => Either[E, B]): Either[E, AnnotatedRead[B]] =
    f(value) match {
      case Left(value)  => Left(value)
      case Right(value) => Right(AnnotatedRead(value, annotations))
    }

  def zipWith[B, C](that: AnnotatedRead[B])(f: (A, B) => C): AnnotatedRead[C] =
    AnnotatedRead(f(value, that.value), self.annotations ++ that.annotations)
}

object AnnotatedRead {
  sealed trait Annotation
  object Annotation {
    case object NonDefaultValue extends Annotation
  }
}
