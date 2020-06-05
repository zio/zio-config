package zio.config.derivation

import scala.annotation.StaticAnnotation

final case class describe(describe: String) extends StaticAnnotation
final case class name(name: String)         extends StaticAnnotation
