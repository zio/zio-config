package zio.config.examples

import zio.config._

import Config._

object TupleExample extends App {
  val config: Config[(String, Int)] =
    (string("a") zip int("b"))

  val source: ConfigSource = ConfigProvider.fromMap(Map("a" -> "a", "b" -> "1"))

  assert(read(config from source) equalM (("a", 1)))
}
