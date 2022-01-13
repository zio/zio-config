package zio.config

import zio._
import zio.test.{Live, TestAspect, TestAspectAtLeastR, TestConfig, ZIOSpecDefault}

abstract class BaseSpec extends ZIOSpecDefault {
  override def aspects: Chunk[TestAspectAtLeastR[Live with TestConfig]] =
    Chunk(TestAspect.timeout(60.seconds) @@ TestAspect.samples(10))
}
