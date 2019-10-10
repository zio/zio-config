package zio.config.actions

import zio.config.ConfigDescriptor

object Write {
  final def write[A](config: ConfigDescriptor[A], a: A): Either[String, Map[String, String]] =
    config match {
      case ConfigDescriptor.Empty() =>
        Right(Map.empty)

      case ConfigDescriptor.Source(path, propertyType) =>
        Right(Map(path -> propertyType.write(a)))

      case ConfigDescriptor.Describe(c, _) =>
        write(c, a)

      case ConfigDescriptor.Nested(c, _) =>
        write(c, a)

      case ConfigDescriptor.Optional(c) =>
        a.fold(Right(Map.empty[String, String]): Either[String, Map[String, String]])(aa => write(c, aa))

      case ConfigDescriptor.Default(c, _) =>
        write(c, a)

      case ConfigDescriptor.XmapEither(c, _, to) =>
        to(a) match {
          case Right(before) =>
            write(c, before)
          case Left(e) =>
            Left(e)
        }

      case ConfigDescriptor.OrElseEither(left, right) =>
        a.fold(aa => write(left, aa), b => write(right, b))

      case ConfigDescriptor.Zip(config1, config2) =>
        write(config1, a._1) match {
          case Right(m1) =>
            write(config2, a._2) match {
              case Right(m2) => Right(m1 ++ m2)
              case Left(m1)  => Left(m1)
            }
          case Left(e) => Left(e)
        }
    }
}
