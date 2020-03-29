package zio.config

import zio.duration._
import zio.test.{ DefaultRunnableSpec, TestAspect, TestAspectAtLeastR, ZSpec }
import zio.test.environment.{ Live, TestEnvironment }

abstract class BaseSpec(override val spec: ZSpec[TestEnvironment, Any]) extends DefaultRunnableSpec {
  override def aspects: List[TestAspectAtLeastR[Live]] = List(TestAspect.timeout(60.seconds))
}
