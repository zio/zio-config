package zio.config.examples

import zio.config.magnolia.{ name, _ }
import zio.config.gen._

object RandomConfigGenerationSimpleExample extends App {
  sealed trait Cloud

  object Cloud {
    case object Aws                                           extends Cloud
    case object Azure                                         extends Cloud
    case class Gcp(@name("DOMAIN") domain: String, e: String) extends Cloud

  }

  sealed trait Region

  @name("ap-southeast-2")
  case object ApSouthEast2 extends Region

  @name("us-east")
  case object UsEast extends Region

  final case class MyConfig(username: String, region: Region)

  println(generateConfigJson(descriptor[MyConfig], 1).unsafeRunChunk)

  // yields for example

  // Chunk(
  //   {
  //    "region" : "ap-southeast-2",
  //     "username" : "eU2KlfATwYZ5s0Y"
  //   }
  // )
}
