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

object IndexedFlat {
  def from(map: Map[Chunk[KeyComponent], String]) =
    new IndexedFlat {
      val escapedSeqDelim                                     = java.util.regex.Pattern.quote(seqDelim)
      val escapedPathDelim                                    = java.util.regex.Pattern.quote(pathDelim)
      def makePathString(path: Chunk[String]): String         = path.mkString(pathDelim)
      def unmakePathString(pathString: String): Chunk[String] = Chunk.fromArray(pathString.split(escapedPathDelim))

      def load[A](path: Chunk[String], primitive: Config.Primitive[A])(implicit
        trace: Trace
      ): IO[Config.Error, Chunk[A]] = {
        val pathString  = makePathString(path)
        val name        = path.lastOption.getOrElse("<unnamed>")
        val description = primitive.description
        val valueOpt    = map.get(pathString)

        for {
          value   <- ZIO
                       .fromOption(valueOpt)
                       .mapError(_ => Config.Error.MissingData(path, s"Expected ${pathString} to be set in properties"))
          results <- Flat.util.parsePrimitive(value, path, name, primitive, escapedSeqDelim)
        } yield results
      }

      def enumerateChildren(path: Chunk[String])(implicit trace: Trace): IO[Config.Error, Set[String]] =
        ZIO.succeed {
          val keyPaths = Chunk.fromIterable(map.keys).map(unmakePathString)

          keyPaths.filter(_.startsWith(path)).map(_.drop(path.length).take(1)).flatten.toSet
        }
    }

}
