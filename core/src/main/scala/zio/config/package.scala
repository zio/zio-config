package zio

import scala.annotation.tailrec

package object config extends ReadFunctions with WriteFunctions with ConfigDocsFunctions {
  final def config[A]: ZIO[Config[A], Nothing, A] =
    ZIO.access(_.config.config)

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

  private[config] def seqEitherCons[A, B](either: ::[Either[A, B]]): Either[A, ::[B]] = {
    val reversed = either.reverse
    reversed.tail.foldLeft(reversed.head.map(singleton))((b, a) => a.flatMap(aa => b.map(bb => ::(aa, bb))))
  }

  // Missing an FP lib here
  private[config] def seqMap[K, A](either: Map[K, Option[A]]): Option[Map[K, A]] =
    either.foldRight(Some(Map.empty[K, A]): Option[Map[K, A]])(
      (a, b) => a._2.flatMap(aa => b.map(bb => bb.updated(a._1, aa)))
    )

  private[config] def seqOption[A](either: List[Option[A]]): Option[List[A]] =
    either.foldRight(Some(List.empty[A]): Option[List[A]])((a, b) => a.flatMap(aa => b.map(bb => aa :: bb)))

  private[config] def mapCons[A, B](a: ::[A])(f: A => B): ::[B] = {
    val list = a.map(f)
    ::(list.head, list.tail)
  }

  private[config] def withIndex[A](a: ::[A]): ::[(A, Int)] = {
    val list = a.zipWithIndex
    ::(list.head, list.tail)
  }

  private[config] def zipCons[A, B](a: ::[A], b: ::[B]): ::[(A, B)] = {
    val list = a.zip(b)
    ::(list.head, list.tail)
  }

  private[config] def addCons[A, B](a: ::[A], b: ::[A]): ::[A] = {
    val list = a ++ b
    ::(list.head, list.tail)
  }

  private[config] final def foreach[R, E, A, B](in: ::[A])(f: A => ZIO[R, E, B]): ZIO[R, E, ::[B]] = {
    val reversesd = in.reverse

    reversesd.tail.foldLeft[ZIO[R, E, ::[B]]](f(reversesd.head).map(singleton)) { (io, a) =>
      f(a).zipWith(io)((b, bs) => ::(b, bs))
    }
  }

  object KeyConversion {
    private def camelToDelimiter(delimiter: String): String => String = s => {
      @tailrec def loop(s: String, output: String, lastUppercase: Boolean): String =
        if (s.isEmpty) output
        else {
          val c = if (s.head.isUpper && !lastUppercase) delimiter + s.head.toLower else s.head.toLower
          loop(s.tail, output + c, s.head.isUpper && !lastUppercase)
        }

      loop(s, "", lastUppercase = true)
    }

    val camelToKebab: String => String =
      camelToDelimiter("-")

    val camelToSnake: String => String =
      camelToDelimiter("_")

    def addPrefix(prefix: String): String => String =
      s => s"${prefix}${s}"

    def postFix(string: String): String => String =
      s => s"${s}${string}"

  }
}
