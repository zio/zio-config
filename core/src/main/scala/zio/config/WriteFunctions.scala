package zio.config

import zio.config.PropertyTree.Record

private[config] trait WriteFunctions {

  final def write[K, V, A](config: ConfigDescriptor[K, V, A], a: A): Either[String, PropertyTree[K, V]] = {
    def go[B](config: ConfigDescriptor[K, V, B], b: B): Either[String, PropertyTree[K, V]] =
      config match {
        case ConfigDescriptor.Source(_, propertyType) =>
          Right(PropertyTree.Leaf(propertyType.write(b)))

        case ConfigDescriptor.Describe(c, _) =>
          go(c, b)

        case cd: ConfigDescriptor.DynamicMap[K, V, a] =>
          val bs = (b: Map[K, a]).toList.map(t => (t._1 -> go(cd.config, t._2)))
          seqMap(bs.toMap).map(t => Record(t))

        case ConfigDescriptor.Nested(parent, c) =>
          go(c, b) match {
            case Right(prop) => Right(PropertyTree.Record(Map(parent -> prop)))
            case Left(v)     => Left(v)
          }

        case cd: ConfigDescriptor.Sequence[K, V, a] =>
          val bs = (b: List[a]).map(go(cd.config, _))
          seqEither[String, PropertyTree[K, V]](bs).map(PropertyTree.Sequence(_))

        case ConfigDescriptor.Optional(c) =>
          b.fold(
            Right(PropertyTree.empty): Either[String, PropertyTree[K, V]]
          )(go(c, _))

        case ConfigDescriptor.Default(c, _) =>
          go(c, b)

        case ConfigDescriptor.XmapEither(c, _, to) => {
          to(b) match {
            case Right(before) =>
              go(c, before)
            case Left(e) =>
              Left(e)
          }
        }

        case ConfigDescriptor.OrElseEither(left, right) => {
          b.fold(
            aa => go(left, aa),
            b => go(right, b)
          )
        }

        case ConfigDescriptor.OrElse(left, right) =>
          go(left, b) match {
            case Right(a) =>
              Right(a)

            case Left(_) =>
              go(right, b)

          }

        case cd: ConfigDescriptor.Zip[K, V, a, b] =>
          val tuple: (a, b) = b

          val leftResult  = go(cd.left, tuple._1)
          val rightResult = go(cd.right, tuple._2)

          for {
            left   <- leftResult
            right  <- rightResult
            merged = left.condense.merge(right.condense)
            result <- merged.headOption
                       .toRight("Failed to write the config back to property tree, at zip node")
          } yield result
      }
    go(config, a)
  }
}
