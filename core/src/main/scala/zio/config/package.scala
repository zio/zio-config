package zio

package object config extends KeyConversionFunctions with ConfigStringModule {
  implicit class MapOps[A](a: => A) {
    def toMap(config: ConfigDescriptor[A], keyDelimiter: String = "."): Either[String, Map[String, ::[String]]] =
      write(config, a).map(_.flattenString(keyDelimiter))
  }

  private[config] def concat[A](l: ::[A], r: ::[A]): ::[A] =
    ::(l.head, l.tail ++ r)

  private[config] def singleton[A](a: A): ::[A] = ::(a, Nil)

  private[config] def seqEither[A, B](either: List[Either[A, B]]): Either[A, List[B]] =
    either.foldRight(Right(List.empty[B]): Either[A, List[B]])((a, b) => a.flatMap(aa => b.map(bb => aa :: bb)))

  private[config] def seqOption[A](options: List[Option[A]]): Option[List[A]] =
    options.foldRight(Some(Nil): Option[List[A]])((a, b) => a.flatMap(aa => b.map(bb => aa :: bb)))

  private[config] def seqMap[K, A, B](map: Map[K, Either[A, B]]): Either[A, Map[K, B]] =
    map.foldRight(Right(Map.empty[K, B]): Either[A, Map[K, B]])(
      (a, b) => a._2.flatMap(aa => b.map(bb => bb.updated(a._1, aa)))
    )

  private[config] def seqMap2[K, E, B](
    map: Map[K, Either[E, B]]
  ): Either[List[E], Map[K, B]] =
    map.foldLeft(
      Right(Map.empty: Map[K, B]): Either[List[E], Map[K, B]]
    ) {
      case (Left(es), (_, Left(e)))   => Left(e :: es)
      case (Left(cs), (_, Right(_)))  => Left(cs)
      case (Right(_), (_, Left(e)))   => Left(e :: Nil)
      case (Right(bs), (k, Right(b))) => Right(bs.updated(k, b))
    }

  private[config] def seqEither2[A, B, C](genError: (Int, A) => C)(list: List[Either[A, B]]): Either[List[C], List[B]] =
    list.zipWithIndex
      .foldLeft(Right(Nil): Either[List[C], List[B]]) {
        case (Left(cs), (Left(a), index)) => Left(genError(index, a) :: cs)
        case (Left(cs), (Right(_), _))    => Left(cs)
        case (Right(_), (Left(a), index)) => Left(genError(index, a) :: Nil)
        case (Right(bs), (Right(b), _))   => Right(b :: bs)
      }
      .map(_.reverse)
}
