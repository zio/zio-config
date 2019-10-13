package zio.config.actions

import zio.config.{ ConfigDescriptor, PropertyTree }

object Write {
  final def write[A](config: ConfigDescriptor[A], a: A): Either[String, PropertyTree[String, String]] = {
    def go[B](prevPath: String, config: ConfigDescriptor[B], b: B): Either[String, PropertyTree[String, String]] =
      config match {
        case ConfigDescriptor.Empty() =>
          Right(PropertyTree.Empty)

        case ConfigDescriptor.Source(path, propertyType) =>
          Right(PropertyTree.Record(Map(path -> PropertyTree.Leaf(propertyType.write(b)))))

        case ConfigDescriptor.Describe(c, _) =>
          go(prevPath, c, b)

        case ConfigDescriptor.Nested(c, parent) =>
          go(prevPath, c, b) match {
            case Right(prop) =>
              prop match {
                case PropertyTree.Record(tree) => Right(PropertyTree.Record(Map(parent -> PropertyTree.Record(tree))))
                case PropertyTree.Empty        => Right(PropertyTree.Record(Map(parent -> PropertyTree.Empty)))
                case PropertyTree.Leaf(value)  => Right(PropertyTree.Record(Map(parent -> PropertyTree.Leaf(value))))
              }
            case Left(v) => Left(v)
          }

        case ConfigDescriptor.Optional(c) =>
          b.fold({
            Right(PropertyTree.Empty): Either[String, PropertyTree[String, String]]
          })(bb => go(prevPath, c, bb))

        case ConfigDescriptor.Default(c, _) =>
          go(prevPath, c, b)

        case ConfigDescriptor.XmapEither(c, _, to) =>
          to(b) match {
            case Right(before) =>
              go(prevPath, c, before)
            case Left(e) =>
              Left(e)
          }

        case ConfigDescriptor.OrElseEither(left, right) =>
          b.fold(aa => go(prevPath, left, aa), b => go(prevPath, right, b))

        case ConfigDescriptor.Zip(config1, config2) =>
          go(prevPath, config1, b._1) match {
            case Right(m1) =>
              go(prevPath, config2, b._2) match {
                case Right(m2) =>
                  m1 match {
                    case PropertyTree.Record(mm) =>
                      m2 match {
                        case PropertyTree.Record(m2) => Right(PropertyTree.Record(mm ++ m2))
                        case PropertyTree.Empty      => Right(m1)
                        case PropertyTree.Leaf(v)    => Right(PropertyTree.Record(Map(prevPath -> PropertyTree.Leaf(v))))
                      }
                    case PropertyTree.Leaf(v) =>
                      m2 match {
                        case PropertyTree.Record(mm) =>
                          Right(PropertyTree.Record(mm ++ Map(prevPath -> PropertyTree.Leaf(v))))
                        case PropertyTree.Empty   => Right(m1)
                        case PropertyTree.Leaf(v) => Right(PropertyTree.Record(Map(prevPath -> PropertyTree.Leaf(v))))
                      }
                    case PropertyTree.Empty =>
                      Right(m2)
                  }
                case Left(m1) => Left(m1)
              }
            case Left(e) => Left(e)
          }
      }
    go("", config, a)
  }
}
