package zio.config

private[config] trait WriteFunctions {
  final def write[K, V, A](config: ConfigDescriptor[K, V, A], a: A): Either[String, PropertyTree[K, V]] = {
    def go[B](config: ConfigDescriptor[K, V, B], b: B): Either[String, PropertyTree[K, V]] =
      config match {
        case ConfigDescriptor.Empty() =>
          Right(PropertyTree.Empty)

        case ConfigDescriptor.Source(path, _, propertyType) =>
          Right(PropertyTree.Record(Map(path -> PropertyTree.Leaf(propertyType.write(b)))))

        case ConfigDescriptor.Describe(c, _) =>
          go(c, b)

        case ConfigDescriptor.Nested(c, parent) =>
          go(c, b) match {
            case Right(prop) => Right(PropertyTree.Record(Map(parent -> prop)))
            case Left(v)     => Left(v)
          }

        case ConfigDescriptor.Optional(c) =>
          b.fold({
            Right(PropertyTree.Empty): Either[String, PropertyTree[K, V]]
          })(bb => go(c, bb))

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
          b.fold(aa => go(left, aa), b => go(right, b))

        case ConfigDescriptor.Zip(config1, config2) =>
          go(config1, b._1) match {
            case Right(m1) =>
              go(config2, b._2) match {
                case Right(m2) =>
                  m1 match {
                    case PropertyTree.Record(mm) =>
                      m2 match {
                        case PropertyTree.Record(mapp) =>
                          // To get over the GADT Skolem with Map
                          val r = mm.toList ++ mapp.toList

                          Right(PropertyTree.Record(r.toMap))
                        case PropertyTree.Empty   => Right(m1)
                        case PropertyTree.Leaf(v) => Right(PropertyTree.Leaf(v))
                      }
                    case PropertyTree.Leaf(_) =>
                      m2 match {
                        case PropertyTree.Record(mm) =>
                          Right(PropertyTree.Record(mm))
                        case PropertyTree.Empty   => Right(m1)
                        case PropertyTree.Leaf(a) => Right(PropertyTree.Leaf(a))
                      }
                    case PropertyTree.Empty =>
                      Right(m2)
                  }
                case Left(m1) => Left(m1)
              }
            case Left(e) => Left(e)
          }
      }
    go(config, a)
  }
}
