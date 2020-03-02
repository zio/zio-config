package zio

import scala.annotation.tailrec

package object config extends ReadFunctions with WriteFunctions with ConfigDocsFunctions {
  final type ReadErrors[K]       = ::[ReadError[K]]
  final type ReadErrorsVector[K] = ReadErrors[Vector[K]]

  final def config[A]: ZIO[Config[A], Nothing, A] =
    ZIO.access(_.config.config)

  private[config] def concat[A](l: ::[A], r: ::[A]): ::[A] =
    ::(l.head, l.tail ++ r)

  private[config] def singleton[A](a: A): ::[A] = ::(a, Nil)

  private[config] def seqEither[A, B](either: List[Either[A, B]]): Either[A, List[B]] =
    either.foldRight(Right(List.empty[B]): Either[A, List[B]])((a, b) => a.flatMap(aa => b.map(bb => aa :: bb)))

  private[config] def seqEitherCons[A, B](either: ::[Either[A, B]]): Either[A, ::[B]] = {
    val reversed = either.reverse
    reversed.tail.foldLeft(reversed.head.map(singleton))((b, a) => a.flatMap(aa => b.map(bb => ::(aa, bb))))
  }

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
