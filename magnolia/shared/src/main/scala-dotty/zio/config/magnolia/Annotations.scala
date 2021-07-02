package zio.config.magnolia

import scala.annotation.StaticAnnotation

final case class describe(describe: String) extends StaticAnnotation

final case class name(name: String) extends StaticAnnotation

final case class names(names: String*) extends StaticAnnotation

final case class label(fieldName: String = "type") extends StaticAnnotation
