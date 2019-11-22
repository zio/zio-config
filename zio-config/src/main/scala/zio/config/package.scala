package zio

package object config extends ReadFunctions with WriteFunctions with ConfigDocsFunctions {
  final type ReadErrors[K, V]       = ::[ReadError[K, V]]
  final type ReadErrorsVector[K, V] = ReadErrors[Vector[K], V]

  final def config[A]: ZIO[Config[A], Nothing, A] =
    ZIO.accessM(_.config.config)

  private[config] def concat[A](l: ::[A], r: ::[A]): ::[A] =
    ::(l.head, l.tail ++ r)

  private[config] def singleton[A](a: A): ::[A] = ::(a, Nil)
}
