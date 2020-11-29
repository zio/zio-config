package zio.config.examples

import zio.config.derivation.name
import zio.config.magnolia._, zio.config.gen._

object RandomConfigGenerationSimpleExample extends App {
  sealed trait Region

  @name("ap-southeast-2")
  case object ApSouthEast2 extends Region

  @name("us-east")
  case object UsEast extends Region

  final case class MyConfig(username: String, region: Region)

  println(generateConfigJson(descriptor[MyConfig]).unsafeRunChunk)

  // yields for example

  // Chunk(
  //   {
  //    "region" : "ap-southeast-2",
  //     "username" : "eU2KlfATwYZ5s0Y"
  //   }
  // )
}
