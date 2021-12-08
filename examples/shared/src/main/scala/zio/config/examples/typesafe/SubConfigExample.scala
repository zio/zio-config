package zio.config.examples.typesafe

import zio.config._, magnolia._
import zio.config.typesafe.TypesafeConfigSource

object SubConfigExample extends App {
  final case class ShortConfig(x: Int, y: Int)

  val config =
    s"""
       | {
       |    "a" : {
       |        "b" : {
       |            "c" : [
       |                {
       |                    "x"  : 1
       |                    "y" : 2
       |                }
       |                 {
       |                    "x" : 3
       |                    "y" : 4
       |                }
       |             ]
       |         }
       |     }
       | }
       |""".stripMargin

  assert(
    read(
      descriptor[ShortConfig] from TypesafeConfigSource.fromHoconString(config).at(path"a.b.c[0]")
    ) equalM (ShortConfig(1, 2))
  )
}
