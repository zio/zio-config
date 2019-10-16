package zio.config.actions

import zio.config.{ ConfigDescriptor, ConfigSource, ReadErrors }
import zio.{ config, ZIO }

object Read {
  // Read
  final def read[A](
    configuration: ConfigDescriptor[A]
  ): ZIO[ConfigSource[String, String], ReadErrors[String, String], A] = {
    def loop[B](
      configuration: ConfigDescriptor[B],
      previousDescription: String,
      paths: List[String]
    ): ZIO[ConfigSource[String, String], ReadErrors[String, String], B] =
      configuration match {
        case ConfigDescriptor.Empty() => ZIO.succeed(None)

        case ConfigDescriptor.Source(path, propertyType) =>
          for {
            configValue <- config
                            .getConfigValue[String, String](paths :+ path)
                            .mapError(ReadErrors(_))

            result <- ZIO.fromEither(
                       propertyType
                         .read(path, configValue.value)
                         .fold(r => Left(ReadErrors(r)), e => Right(e))
                     )

          } yield result

        case ConfigDescriptor.Nested(c, path) =>
          loop(c, previousDescription, paths :+ path)

        case ConfigDescriptor.XmapEither(c, f, _) =>
          loop(c, previousDescription, paths).flatMap { a =>
            ZIO.fromEither(f(a)).mapError(err => ReadErrors(err))
          }

        // No need to add report on the default value.
        case ConfigDescriptor.Default(c, value) =>
          loop(c, previousDescription, paths).fold(
            _ => value,
            identity
          )

        case ConfigDescriptor.Describe(c, message) =>
          loop(c, message, paths)

        case ConfigDescriptor.Optional(c) =>
          loop(c, previousDescription, paths).option

        case ConfigDescriptor.Zip(left, right) =>
          loop(left, previousDescription, paths).either
            .flatMap(
              res1 =>
                loop(right, previousDescription, paths).either.map(
                  res2 =>
                    (res1, res2) match {
                      case (Right(a), Right(b))     => Right((a, b))
                      case (Left(a), Right(_))      => Left(a)
                      case (Right(_), Left(error))  => Left(error)
                      case (Left(err1), Left(err2)) => Left(ReadErrors.concat(err1, err2))
                    }
                )
            )
            .absolve

        case ConfigDescriptor.OrElseEither(left, right) =>
          loop(left, previousDescription, paths).either.flatMap(
            {
              case Right(a) => ZIO.succeed(Left(a))
              case Left(lerr) =>
                loop(right, previousDescription, paths).either.flatMap(
                  {
                    case Right(b)   => ZIO.succeed(Right(b))
                    case Left(rerr) => ZIO.fail(ReadErrors.concat(lerr, rerr))
                  }
                )
            }
          )
      }

    loop(configuration, "", Nil)
  }

  final def readWithConfigDocs[A](
    configuration: ConfigDescriptor[A]
  ): ZIO[ConfigSource[String, String], ReadErrors[String, String], (A, ConfigDocs)] = {
    def loop[B](
      configuration: ConfigDescriptor[B],
      previousDescription: String,
      paths: List[String],
      acc: List[String],
      docs: ConfigDocs
    ): ZIO[ConfigSource[String, String], ReadErrors[String, String], (B, ConfigDocs)] =
      configuration match {
        case ConfigDescriptor.Empty() => ZIO.succeed((None, ConfigDocs.Empty()))

        case ConfigDescriptor.Source(path, propertyType) =>
          for {
            configValue <- config
                            .getConfigValue[String, String](paths :+ path)
                            .mapError(ReadErrors(_))

            result <- ZIO.fromEither(
                       propertyType
                         .read(path, configValue.value)
                         .fold(r => Left(ReadErrors(r)), e => Right(e))
                     )
            pathDetails = ConfigDocs.PathDetails(
              paths.toVector :+ path,
              Some(configValue.value),
              if (previousDescription.isEmpty) acc
              else previousDescription :: acc
            )

          } yield (result, pathDetails)

        case ConfigDescriptor.Nested(c, path) =>
          loop(c, previousDescription, paths :+ path, acc, docs)

        case ConfigDescriptor.XmapEither(c, f, _) =>
          loop(c, previousDescription, paths, acc, docs).flatMap { a =>
            ZIO.fromEither(f(a)).bimap(err => ReadErrors(err), res => (res, docs))
          }

        // No need to add report on the default value.
        case ConfigDescriptor.Default(c, value) =>
          loop(c, previousDescription, paths, acc, docs).map(_ => (value, docs))

        case ConfigDescriptor.Describe(c, message) =>
          loop(c, message, paths, acc, docs)

        case ConfigDescriptor.Optional(c) =>
          loop(c, previousDescription, paths, acc, docs).option.map((_, docs))

        case ConfigDescriptor.Zip(left, right) => {
          val zip = for {
            res1 <- loop(left, previousDescription, paths, acc, docs).either
            res2 <- loop(right, previousDescription, paths, acc, docs).either

            zipResult = (res1, res2) match {
              case (Right((a, doc1)), Right((b, doc2))) => Right(((a, b), ConfigDocs.And(doc1, doc2)))
              case (Left(a), Right(_))                  => Left(a)
              case (Right(_), Left(error))              => Left(error)
              case (Left(err1), Left(err2))             => Left(ReadErrors.concat(err1, err2))
            }
          } yield zipResult

          zip.absolve
        }

        case ConfigDescriptor.OrElseEither(left, right) =>
          loop(left, previousDescription, paths, acc, docs).either.flatMap(
            {
              case Right(a) => ZIO.succeed((Left(a), docs))
              case Left(lerr) =>
                loop(right, previousDescription, paths, acc, docs).either.flatMap(
                  {
                    case Right(b)   => ZIO.succeed((Right(b), docs))
                    case Left(rerr) => ZIO.fail(ReadErrors.concat(lerr, rerr))
                  }
                )
            }
          )
      }

    loop(configuration, "", Nil, Nil, ConfigDocs.Empty())
  }
}
