package zio.config

import zio.system.System
import zio.ZIO

final case class Details(key: String, value: String, description: String) {
  override def toString: String =
    s"path:$key, value:$value, description:$description"
}

final case class ConfigReport(list: List[Details]) {

  def addDetails(details: Details): ConfigReport =
    ConfigReport(details :: list)

  def ++(c: ConfigReport): ConfigReport =
    ConfigReport(list ++ c.list)

  def showDetails: ZIO[System, Nothing, String] =
    ZIO.accessM[System](_.system.lineSeparator).map { sep =>
      list.mkString(sep)
    }
}
