package zio.config

private[config] trait WriteFunctions {
  final def write[K, V, A](config: ConfigDescriptor[K, V, A], a: A): Either[String, PropertyTree[K, V]] = {
    def go[B](config: ConfigDescriptor[K, V, B], b: B): Either[String, PropertyTree[K, V]] =
      config match {
        case ConfigDescriptor.Source(path, _, propertyType) =>
          Right(PropertyTree.Record(Map(path -> PropertyTree.Leaf(propertyType.write(b)))))

        case ConfigDescriptor.Describe(c, _) =>
          go(c, b)

        case ConfigDescriptor.Nested(parent, c) =>
          go(c, b) match {
            case Right(prop) => Right(PropertyTree.Record(Map(parent -> prop)))
            case Left(v)     => Left(v)
          }

        case ConfigDescriptor.Sequence(c) =>
          seqEither(b.map(eachB => {
            go(c, eachB)
          })).map(PropertyTree.Sequence(_))

        case ConfigDescriptor.Optional(c) =>
          b.fold({
            Right(PropertyTree.Empty): Either[String, PropertyTree[K, V]]
          })(bb => {
            go(c, bb)
          })

        case ConfigDescriptor.Default(c, _) =>
          go(c, b)

        case ConfigDescriptor.XmapEither(c, _, to) =>
          to(b) match {
            case Right(before) =>
              go(c, before)
            case Left(e) =>
              Left(e)
          }

        case ConfigDescriptor.OrElseEither(left, right) =>
          b.fold(aa => { go(left, aa) }, b => { go(right, b) })

        case ConfigDescriptor.Zip(config1, config2) =>
          go(config1, b._1) match {
            case Right(m1) =>
              go(config2, b._2) match {
                case Right(m2) =>
                  Right(m1.merge(m2))
                case Left(m1) =>
                  Left(m1)
              }
            case Left(e) => Left(e)
          }
      }
    go(config, a)
  }
}
