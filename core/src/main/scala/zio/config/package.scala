package zio

package object config extends ReadFunctions with WriteFunctions with ConfigDocsFunctions {
  final type ReadErrors[K, V]       = ::[ReadError[K, V]]
  final type ReadErrorsVector[K, V] = ReadErrors[Vector[K], V]

  final def config[A]: ZIO[Config[A], Nothing, A] =
    ZIO.accessM(_.config.config)

  private[config] def concat[A](l: ::[A], r: ::[A]): ::[A] =
    ::(l.head, l.tail ++ r)

  private[config] def singleton[A](a: A): ::[A] = ::(a, Nil)

  def seqEither[A, B](either: List[Either[A, B]]): Either[A, List[B]] =
    either.foldRight(Right(List.empty[B]): Either[A, List[B]])((a, b) => a.flatMap(aa => b.map(bb => aa :: bb)))

  final def foreach[R, E, A, B](in: ::[A])(f: A => ZIO[R, E, B]): ZIO[R, E, ::[B]] = {
    val reversesd = in.reverse

    reversesd.tail.foldLeft[ZIO[R, E, ::[B]]](f(reversesd.head).map(singleton)) { (io, a) =>
      f(a).zipWith(io)((b, bs) => ::(b, bs))
    }
  }
}
