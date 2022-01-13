package zio.config

import zio._
import zio.test.{Live, TestAspect, TestAspectAtLeastR, ZIOSpecDefault}

abstract class BaseSpec extends ZIOSpecDefault {
  override def aspects: Chunk[TestAspectAtLeastR[Live]] = Chunk(TestAspect.timeout(60.seconds))
}
