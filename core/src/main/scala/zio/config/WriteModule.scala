package zio.config

import zio.config.PropertyTree.Record

private[config] trait WriteModule extends ConfigDescriptorModule {
  import ConfigDescriptorAdt._

  final def write[A](config: ConfigDescriptor[A], a: A): Either[String, PropertyTree[K, V]] = {
    def go[B](config: ConfigDescriptor[B], b: B): Either[String, PropertyTree[K, V]] =
      config match {
        case Source(_, propertyType) =>
          Right(PropertyTree.Leaf(propertyType.write(b)))

        case Describe(c, _) =>
          go(c.value, b)

        case cd: DynamicMap[a] =>
          val bs = (b: Map[K, a]).toList.map(t => (t._1 -> go(cd.config.value, t._2)))
          seqMap(bs.toMap).map(t => Record(t))

        case Nested(_, parent, c) =>
          go(c.value, b) match {
            case Right(prop) => Right(PropertyTree.Record(Map(parent -> prop)))
            case Left(v)     => Left(v)
          }

        case Optional(c) =>
          b.fold(
            Right(PropertyTree.empty): Either[String, PropertyTree[K, V]]
          )(go(c.value, _))

        case cd: Sequence[a] =>
          val bs = (b: List[a]).map(go(cd.config.value, _))
          seqEither[String, PropertyTree[K, V]](bs).map(PropertyTree.Sequence(_))

        case Default(c, _) =>
          go(c.value, b)

        case TransformOrFail(c, _, to) => {
          to(b) match {
            case Right(before) =>
              go(c.value, before)
            case Left(e) =>
              Left(e)
          }
        }

        case OrElseEither(left, right) => {
          b.fold(
            aa => go(left.value, aa),
            b => go(right.value, b)
          )
        }

        case OrElse(left, right) =>
          go(left.value, b) match {
            case Right(a) =>
              Right(a)

            case Left(_) =>
              go(right.value, b)

          }

        case cd: Zip[a, b] =>
          val tuple: (a, b) = b

          val leftResult  = go(cd.left.value, tuple._1)
          val rightResult = go(cd.right.value, tuple._2)

          for {
            left   <- leftResult
            right  <- rightResult
            merged = left.merge(right)
            result <- merged.headOption.toRight("Failed to write the config back to property tree, at zip node")
          } yield result
      }
    go(config, a)
  }
}
