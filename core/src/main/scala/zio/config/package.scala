package zio

package object config
    extends ReadFunctions
    with WriteFunctions
    with ConfigDocsFunctions
    with EitherFunctions
    with KeyConversionFunctions {

  type Config[A] = Has[A]

  type NonEmptyList[A] = ::[A]

  object NonEmptyList {
    def apply[A](a: A*) =
      ::(a.head, a.tail.toList)
  }

  final def config[A](implicit tagged: Tagged[A]): ZIO[Config[A], Nothing, A] =
    ZIO.access(_.get)

  private[config] def concat[A](l: ::[A], r: ::[A]): ::[A] =
    ::(l.head, l.tail ++ r)

  private[config] def singleton[A](a: A): ::[A] = ::(a, Nil)

  private[config] def seqEither[A, B](either: List[Either[A, B]]): Either[A, List[B]] =
    either.foldRight(Right(List.empty[B]): Either[A, List[B]])((a, b) => a.flatMap(aa => b.map(bb => aa :: bb)))

  private[config] def seqEither2[A, B, C](genError: (Int, A) => C)(list: List[Either[A, B]]): Either[List[C], List[B]] =
    list.zipWithIndex
      .foldLeft(Right(Nil): Either[List[C], List[B]]) {
        case (Left(cs), (Left(a), index))   => Left(genError(index, a) :: cs)
        case (Left(cs), (Right(b), index))  => Left(cs)
        case (Right(bs), (Left(a), index))  => Left(genError(index, a) :: Nil)
        case (Right(bs), (Right(b), index)) => Right(b :: bs)
      }
      .map(_.reverse)

  private[config] final def foreach[R, E, A, B](in: ::[A])(f: A => ZIO[R, E, B]): ZIO[R, E, ::[B]] = {
    val reversesd = in.reverse

    reversesd.tail.foldLeft[ZIO[R, E, ::[B]]](f(reversesd.head).map(singleton)) { (io, a) =>
      f(a).zipWith(io)((b, bs) => ::(b, bs))
    }
  }
}
