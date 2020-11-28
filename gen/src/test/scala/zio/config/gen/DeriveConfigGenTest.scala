package zio.config.gen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.config.magnolia._

// GenerateCustomConfigTest is in a custom package so as to not import the inbuilt instances available through gen package
class GenerateConfigTest extends AnyFlatSpec with Matchers {

  "A generator" should "successfully generate values based on config descriptor" in {
     assert(GenerateConfigTestUtils.result.head === "")
  }

}

object GenerateConfigTestUtils {
  sealed trait Region

  @name("ap-southeast")
  case object ApSouthEast2 extends Region

  @name("usEast")
  case object UsEast extends Region

  final case class Database(port: Int, url: String)
  final case class MyConfig(region: Region, database: Database)

  val result =
    generateConfigJson(descriptor[MyConfig]).unsafeRunChunk(1)


}
