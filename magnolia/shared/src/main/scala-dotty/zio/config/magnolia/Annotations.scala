package zio.config.magnolia

import scala.annotation.StaticAnnotation

final case class describe(describe: String) extends StaticAnnotation

final case class name(name: String) extends StaticAnnotation

final case class names(names: String*) extends StaticAnnotation

object names {
  def fromListOfName(list: List[name]): names =
    names(list.map(_.name):_*)

  def fromListOfNames(list: List[names]): names =
    names(list.flatMap(names => names.names.toList):_*)
}

final case class label(fieldName: String = "type") extends StaticAnnotation
