package zio.config.syntax

import zio._

trait IndexedFlat { self =>
  def load[A](path: Chunk[KeyComponent], config: Config.Primitive[A])(implicit trace: Trace): IO[Config.Error, Chunk[A]]

  def enumerateChildren(path: Chunk[KeyComponent])(implicit trace: Trace): IO[Config.Error, Set[Chunk[KeyComponent]]]

  final def orElse(that: IndexedFlat): IndexedFlat =
    new IndexedFlat {
      def load[A](path: Chunk[KeyComponent], config: Config.Primitive[A])(implicit
        trace: Trace
      ): IO[Config.Error, Chunk[A]]                                        =
        self.load(path, config).catchAll(e1 => that.load(path, config).catchAll(e2 => ZIO.fail(e1 || e2)))
      def enumerateChildren(
        path: Chunk[KeyComponent]
      )(implicit trace: Trace): IO[Config.Error, Set[Chunk[KeyComponent]]] =
        for {
          l      <- self.enumerateChildren(path).either
          r      <- that.enumerateChildren(path).either
          result <- (l, r) match {
                      case (Left(e1), Left(e2)) => ZIO.fail(e1 && e2)
                      case (Left(e1), Right(_)) => ZIO.fail(e1)
                      case (Right(_), Left(e2)) => ZIO.fail(e2)
                      case (Right(l), Right(r)) => ZIO.succeed(l ++ r)
                    }
        } yield result
    }

  final def nested(name: KeyComponent): IndexedFlat =
    new IndexedFlat {
      def load[A](path: Chunk[KeyComponent], config: Config.Primitive[A])(implicit
        trace: Trace
      ): IO[Config.Error, Chunk[A]]                 =
        self.load(name +: path, config)
      def enumerateChildren(path: Chunk[KeyComponent])(implicit
        trace: Trace
      ): IO[Config.Error, Set[Chunk[KeyComponent]]] =
        self.enumerateChildren(name +: path)
    }
}
