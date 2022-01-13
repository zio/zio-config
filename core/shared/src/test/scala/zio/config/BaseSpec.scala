package zio.config

import zio.test.{TestAspect, TestAspectAtLeastR}
import zio._
import zio.test.{Live, ZIOSpecDefault}

abstract class BaseSpec extends ZIOSpecDefault {
  override def aspects: Chunk[TestAspectAtLeastR[Live]] = Chunk(TestAspect.timeout(60.seconds))
}
