package zio.config

final case class Details(key: String, value: String, description: String)

final case class ConfigReport(list: List[Details]) {

  def addDetails(details: Details): ConfigReport =
    ConfigReport(details :: list)

  def ++(c: ConfigReport): ConfigReport =
    ConfigReport(list ++ c.list)
}
