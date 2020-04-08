package zio.config

private[config] trait WriteFunctions {

  final def write[A](config: ConfigDescriptor[A], a: A): Either[String, PropertyTree] = {
    def go[B](config: ConfigDescriptor[B], b: B): Either[String, PropertyTree] =
      config match {
        case ConfigDescriptor.Source(_, propertyType) =>
          Right(PropertyTree.Leaf(propertyType.write(b)))

        case ConfigDescriptor.Describe(c, _) =>
          go(c, b)

        case ConfigDescriptor.Nested(parent, c) =>
          go(c, b) match {
            case Right(prop) => Right(PropertyTree.Record(Map(parent -> prop)))
            case Left(v)     => Left(v)
          }

        case cd: ConfigDescriptor.Sequence[a] =>
          val bs = (b: List[a]).map(go(cd.config, _))
          seqEither[String, PropertyTree](bs).map(PropertyTree.Sequence)

        case ConfigDescriptor.Optional(c) =>
          b.fold(
            Right(PropertyTree.empty): Either[String, PropertyTree]
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

        case cd: ConfigDescriptor.Zip[a, b] =>
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
