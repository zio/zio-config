package zio.config.examples.typesafe

import zio.config._

import typesafe._
import magnolia._
import zio.ConfigProvider
import zio.Chunk
import zio.config.syntax.KeyComponent

object SubConfigExample extends App {
  final case class ShortConfig(x: Int, y: Int)

  val hoconStr: String =
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
      deriveConfig[ShortConfig] from ConfigProvider.fromHoconString(hoconStr).at(path"a.b.c[0]")
    ) equalM (ShortConfig(1, 2))
  )
}
