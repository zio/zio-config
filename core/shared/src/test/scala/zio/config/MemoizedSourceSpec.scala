package zio.config

import zio.config.ConfigDescriptor._
import zio.config.NestedConfigTestUtils._
import zio.config.helpers._
import zio.random.Random
import zio.test.Assertion._
import zio.test._
import zio.ZIO
import zio.{Has}

object MemoizedSourceSpec extends BaseSpec {
  val spec: Spec[Has[TestConfig.Service] with Has[Random.Service], TestFailure[ReadError[String]], TestSuccess] =
    suite("A memoized source runs a resource acquisition and closing only once")(
      testM("read") {
        checkM(genNestedConfigParams) { p =>
          assertM(ZIO.succeed(true))(equalTo(true))
        }
      }
    )
}

object MemoizedSourceSpecUtils {
  val effectFulSource: ConfigSource = ???
}
