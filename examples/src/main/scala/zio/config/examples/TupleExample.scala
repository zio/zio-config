package zio.config.examples

import zio.config._, ConfigDescriptor._

object TupleExample extends App {
  val config: ConfigDescriptor[(String, Int)] =
    (string("a") |@| int("b")).tupled

  val source = ConfigSource.fromMap(Map("a" -> "a", "b" -> "1"))

  assert(read(config from source) == Right((("a", 1))))
}
