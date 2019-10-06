package zio.config.actions

import zio.config.ConfigDescriptor.Succeed
import zio.config.ConfigDescriptor

sealed trait ConfigDocs

// Man page
object ConfigDocs {

  final case class Empty()                                  extends ConfigDocs
  final case class Leaf(value: KeyDescription)              extends ConfigDocs
  final case class And(left: ConfigDocs, right: ConfigDocs) extends ConfigDocs
  final case class Or(left: ConfigDocs, right: ConfigDocs)  extends ConfigDocs

  final case class KeyDescription(path: String, value: Option[String], docs: List[String])

  final def createDoc[A](config: ConfigDescriptor[A], value: Option[A]): ConfigDocs = {
    def loop[B](
      accumulatedDoc: List[String],
      previousDescription: String,
      config: ConfigDescriptor[B],
      desc: ConfigDocs,
      value: Option[B]
    ): ConfigDocs =
      config match {
        case Succeed(_) => desc
        case ConfigDescriptor.Source(path, p) =>
          ConfigDocs.Leaf(
            KeyDescription(
              path,
              value.map(t => p.write(t)),
              if (previousDescription.isEmpty) accumulatedDoc
              else previousDescription :: accumulatedDoc
            )
          )
        case ConfigDescriptor.Default(c, _) => loop(accumulatedDoc, previousDescription, c, desc, value)
        case ConfigDescriptor.Describe(c, message) =>
          loop(message :: accumulatedDoc, previousDescription, c, desc, value)
        case ConfigDescriptor.Optional(c) =>
          value match {
            case Some(result) =>
              result match {
                case Some(v) => loop(accumulatedDoc, previousDescription, c, desc, Some(v))
                case None    => loop(accumulatedDoc, previousDescription, c, desc, None)
              }
            case None => loop(accumulatedDoc, previousDescription, c, desc, None)
          }
        case ConfigDescriptor.MapEither(c, _, to) =>
          value match {
            case Some(v) =>
              to(v) match {
                case Right(vv) => loop(accumulatedDoc, previousDescription, c, desc, Some(vv))
                case Left(_)   => loop(accumulatedDoc, previousDescription, c, desc, None)
              }
            case None => loop(accumulatedDoc, previousDescription, c, desc, None)
          }

        case ConfigDescriptor.Zip(left, right) =>
          value match {
            case Some(tuple) =>
              ConfigDocs.And(
                loop(accumulatedDoc, previousDescription, left, desc, Some(tuple._1)),
                loop(accumulatedDoc, previousDescription, right, desc, Some(tuple._2))
              )

            case None =>
              ConfigDocs.And(
                loop(accumulatedDoc, previousDescription, left, desc, None),
                loop(accumulatedDoc, previousDescription, right, desc, None)
              )
          }

        case ConfigDescriptor.Or(left, right) =>
          value match {
            case Some(res) =>
              res match {
                case Left(vv) =>
                  ConfigDocs.Or(
                    loop(accumulatedDoc, previousDescription, left, desc, Some(vv)),
                    loop(accumulatedDoc, previousDescription, right, desc, None)
                  )

                case Right(vv) =>
                  ConfigDocs.Or(
                    loop(accumulatedDoc, previousDescription, left, desc, None),
                    loop(accumulatedDoc, previousDescription, right, desc, Some(vv))
                  )
              }

            case None =>
              ConfigDocs.Or(
                loop(accumulatedDoc, previousDescription, left, desc, None),
                loop(accumulatedDoc, previousDescription, right, desc, None)
              )
          }
      }

    loop(Nil, "", config, Empty(), value)
  }
}
