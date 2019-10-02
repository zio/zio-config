package zio.config.actions

import zio.config.ConfigDescriptor.Succeed
import zio.config.ConfigDescriptor
import zio.config.actions.UserManual.KeyDescription

/**
 * A config description is a description of all keys, and the description of another config and the description of...
 * where only one of them need to exist for successful parsing (Config.Or).
 */
final case class UserManual(keyDescriptions: List[KeyDescription], or: Option[UserManual])

// Man page
object UserManual {
  final def man[A](config: ConfigDescriptor[A]): UserManual = {
    def loop[B](
      acc: List[String],
      previousDescription: String,
      config: ConfigDescriptor[B],
      desc: UserManual
    ): UserManual =
      config match {
        case Succeed(_) => desc
        case ConfigDescriptor.Source(path, _) =>
          UserManual(
            List(KeyDescription(path, if (previousDescription.isEmpty) acc else previousDescription :: acc)),
            None
          )
        case ConfigDescriptor.Describe(c, message) => loop(message :: acc, previousDescription, c, desc)
        case ConfigDescriptor.Optional(c)          => loop(acc, previousDescription, c, desc)
        case ConfigDescriptor.MapEither(c, _, _)   => loop(acc, previousDescription, c, desc)
        case ConfigDescriptor.OnError(c, _)        => loop(acc, previousDescription, c, desc)
        case ConfigDescriptor.Zip(left, right) =>
          UserManual(
            loop(acc, previousDescription, left, desc).keyDescriptions ++ loop(acc, previousDescription, right, desc).keyDescriptions,
            None
          )
        case ConfigDescriptor.Or(left, right) =>
          UserManual(
            loop(acc, previousDescription, left, desc).keyDescriptions,
            Some(loop(acc, previousDescription, right, desc))
          )
      }

    loop(Nil, "", config, UserManual(Nil, None))
  }

  case class KeyDescription(path: String, list: List[String])
}
