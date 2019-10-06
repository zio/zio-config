package zio.config.actions

import zio.config.ConfigDescriptor.Succeed
import zio.config.ConfigDescriptor
import zio.config.actions.ConfigDocs.KeyDescription

final case class ConfigDocs(configKeysAndDescription: List[KeyDescription], or: Option[ConfigDocs])

// Man page
object ConfigDocs {
  final case class KeyDescription(path: String, value: Option[String], docs: List[String])

  final def docs[A](config: ConfigDescriptor[A], value: Option[A]): ConfigDocs = {
    def loop[B](
      acc: List[String],
      previousDescription: String,
      config: ConfigDescriptor[B],
      desc: ConfigDocs,
      value: Option[B]
    ): ConfigDocs =
      config match {
        case Succeed(_) => desc
        case ConfigDescriptor.Source(path, p) =>
          ConfigDocs(
            List(
              KeyDescription(
                path,
                value.map(t => p.write(t)),
                if (previousDescription.isEmpty) acc
                else previousDescription :: acc
              )
            ),
            None
          )
        case ConfigDescriptor.Default(c, _)        => loop(acc, previousDescription, c, desc, value)
        case ConfigDescriptor.Describe(c, message) => loop(message :: acc, previousDescription, c, desc, value)
        case ConfigDescriptor.Optional(c) =>
          value match {
            case Some(result) =>
              result match {
                case Some(v) => loop(acc, previousDescription, c, desc, Some(v))
                case None    => loop(acc, previousDescription, c, desc, None)
              }
            case None => loop(acc, previousDescription, c, desc, None)
          }
        case ConfigDescriptor.MapEither(c, _, to) =>
          value match {
            case Some(v) =>
              to(v) match {
                case Right(vv) => loop(acc, previousDescription, c, desc, Some(vv))
                case Left(_)   => loop(acc, previousDescription, c, desc, None)
              }
            case None => loop(acc, previousDescription, c, desc, None)
          }

        case ConfigDescriptor.Zip(left, right) =>
          value match {
            case Some(tuple) =>
              ConfigDocs(
                loop(acc, previousDescription, left, desc, Some(tuple._1)).configKeysAndDescription ++ loop(
                  acc,
                  previousDescription,
                  right,
                  desc,
                  Some(tuple._2)
                ).configKeysAndDescription,
                None
              )

            case None =>
              ConfigDocs(
                loop(acc, previousDescription, left, desc, None).configKeysAndDescription ++ loop(
                  acc,
                  previousDescription,
                  right,
                  desc,
                  None
                ).configKeysAndDescription,
                None
              )
          }

        case ConfigDescriptor.Or(left, right) =>
          value match {
            case Some(res) =>
              res match {
                case Left(vv) =>
                  ConfigDocs(
                    loop(acc, previousDescription, left, desc, Some(vv)).configKeysAndDescription,
                    Some(loop(acc, previousDescription, right, desc, None))
                  )

                case Right(vv) =>
                  ConfigDocs(
                    loop(acc, previousDescription, left, desc, None).configKeysAndDescription,
                    Some(loop(acc, previousDescription, right, desc, Some(vv)))
                  )
              }

            case None =>
              ConfigDocs(
                loop(acc, previousDescription, left, desc, None).configKeysAndDescription,
                Some(loop(acc, previousDescription, right, desc, None))
              )
          }
      }

    loop(Nil, "", config, ConfigDocs(Nil, None), value)
  }
}
