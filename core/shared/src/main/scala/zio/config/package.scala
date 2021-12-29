package zio

package object config extends KeyConversionFunctions with ConfigStringModule with ImplicitTupleConversion {

  implicit class Interpolator(private val sc: StringContext) extends AnyVal {
    def path(str: String*): PropertyTreePath[String] = PropertyTreePath.$(sc.s(str: _*))
  }

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
    map.foldRight(Right(Map.empty[K, B]): Either[A, Map[K, B]])((a, b) =>
      a._2.flatMap(aa => b.map(bb => bb.updated(a._1, aa)))
    )

  private[config] def seqMap2[K, E, B](
    map: Map[K, ZManaged[Any, E, PropertyTree[K, B]]]
  ): ZManaged[Any, List[E], PropertyTree[K, Map[K, B]]] =
    map.foldLeft(
      ZManaged.fromEither[List[E], PropertyTree[K, Map[K, B]]](Right(PropertyTree.Leaf(Map.empty)))
    ) { case (acc, (k, managed)) =>
      (for {
        a <- acc.either
        m <- managed.either
      } yield (a, (k, m)) match {
        case (Left(es), (_, Left(e)))   => Left(e :: es)
        case (Left(cs), (_, Right(_)))  => Left(cs)
        case (Right(_), (_, Left(e)))   => Left(e :: Nil)
        case (Right(bs), (k, Right(b))) => Right(bs.flatMap(map => b.map(leaf => map.updated(k, leaf))))
      }).absolve
    }

  private[config] def seqEither2[K, A, B, C](
    genError: (Int, A) => C
  )(list: List[ZManaged[Any, A, PropertyTree[K, B]]]): ZManaged[Any, List[C], PropertyTree[K, List[B]]] =
    list.zipWithIndex
      .foldLeft(
        ZManaged.fromEither(Right(PropertyTree.Leaf(List.empty))): ZManaged[Any, List[C], PropertyTree[K, List[B]]]
      ) { case (acc, (managed, index)) =>
        (for {
          a <- acc.either
          m <- managed.either
        } yield (a, (m, index)) match {
          case (Left(cs), (Left(a), index)) => Left(genError(index, a) :: cs)
          case (Left(cs), (Right(_), _))    => Left(cs)
          case (Right(_), (Left(a), index)) => Left(genError(index, a) :: Nil)
          case (Right(bs), (Right(b), _))   => Right(bs.flatMap(list => b.map(leaf => leaf :: list)))
        }).absolve
      }
      .map(_.map(_.reverse))
}
