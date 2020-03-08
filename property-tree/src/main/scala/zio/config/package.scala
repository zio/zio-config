package zio

package object config {
  private[config] def singleton[A](a: A): ::[A] = ::(a, Nil)

  private[config] def seqOption[A](either: List[Option[A]]): Option[List[A]] =
    either.foldRight(Some(List.empty[A]): Option[List[A]])((a, b) => a.flatMap(aa => b.map(bb => aa :: bb)))

  private[config] def seqEither[A, B](either: List[Either[A, B]]): Either[A, List[B]] =
    either.foldRight(Right(List.empty[B]): Either[A, List[B]])((a, b) => a.flatMap(aa => b.map(bb => aa :: bb)))

  // Missing an FP lib here
  private[config] def seqMap[K, A](either: Map[K, Option[A]]): Option[Map[K, A]] =
    either.foldRight(Some(Map.empty[K, A]): Option[Map[K, A]])(
      (a, b) => a._2.flatMap(aa => b.map(bb => bb.updated(a._1, aa)))
    )

}
