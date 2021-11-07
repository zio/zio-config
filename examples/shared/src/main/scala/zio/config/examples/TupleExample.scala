package zio.config.examples

import zio.config._

import ConfigDescriptor._

object TupleExample extends App {
  val config: ConfigDescriptor[(String, Int)] =
    (string("a") |@| int("b")).tupled

  val source: ConfigSource = ConfigSource.fromMap(Map("a" -> "a", "b" -> "1"))

  assert(read(config from source) equalM (("a", 1)))
}
