package zio.config

import zio.duration._
import zio.test.environment.Live
import zio.test.{DefaultRunnableSpec, TestAspect, TestAspectAtLeastR}

abstract class BaseSpec extends DefaultRunnableSpec {
  override def aspects: List[TestAspectAtLeastR[Live]] = List(TestAspect.timeout(60.seconds))
}
