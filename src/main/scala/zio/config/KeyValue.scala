package zio.config

import KeyValue._

final case class KeyValue(allConfig: Map[String, String]) {
  def ++(k: KeyValue): KeyValue = KeyValue(allConfig ++ k.allConfig)

  def get(key: String): LookupResult =
    lookupFromOption(key, allConfig.get(key))

  override def toString: String = allConfig.map(t => s"${t._1}=${t._2}").mkString("\n")
}

object KeyValue {
  sealed trait LookupResult

  object LookupResult {
    case object NotFound                         extends LookupResult
    case class Found(key: String, value: String) extends LookupResult
  }

  def lookupFromOption(key: String, value: Option[String]): LookupResult =
    value.fold[LookupResult](LookupResult.NotFound)(value => LookupResult.Found(key, value))
}
