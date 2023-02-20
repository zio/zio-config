package zio.config.examples

import zio.config._
import zio.{Config, ConfigProvider}

import Config._

object TupleExample extends App {
  val config: Config[(String, Int)] =
    (string("a") zip int("b"))

  val source: ConfigProvider = ConfigProvider.fromMap(Map("a" -> "a", "b" -> "1"))

  assert(read(config from source) equalM (("a", 1)))
}
