package zio.config

import zio._
import zio.test.{DefaultRunnableSpec, TestAspect, TestAspectAtLeastR, TestEnvironment}

abstract class BaseSpec extends DefaultRunnableSpec {
  override def aspects: List[TestAspectAtLeastR[TestEnvironment]] = List(TestAspect.timeout(60.seconds))
}
