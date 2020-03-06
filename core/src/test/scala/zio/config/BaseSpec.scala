package zio.config

import zio.duration._
import zio.test.{ DefaultRunnableSpec, TestAspect, ZSpec }
import zio.test.environment.TestEnvironment

abstract class BaseSpec(override val spec: ZSpec[TestEnvironment, Any]) extends DefaultRunnableSpec {
  override def aspects = List(TestAspect.timeout(60.seconds))
}
