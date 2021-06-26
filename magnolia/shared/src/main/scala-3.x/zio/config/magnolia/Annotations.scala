package zio.config.magnolia

/**
 * derived module is confusing, and is non-essential.
 * scala-3 is considered a new module as such and will be independent
 * as much as possible
 */
import scala.annotation.StaticAnnotation

final case class describe(describe: String) extends StaticAnnotation
final case class name(name: String)         extends StaticAnnotation
final case class names(names: String*)      extends StaticAnnotation
