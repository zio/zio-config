package zio.config

import zio.config.ConfigDescriptor.{int, list}
import zio.test.Assertion._
import zio.test.TestEnvironment
import zio.test._
import zio.test.ZIOSpecDefault

object CommandLineListAccumulationTest extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("Configuration of a list from multiple entries")(
      test("Using single arg --key=value style") {
        check(Gen.int(1, 10).map(_ => 1)) { count =>
          val args                                      = renderArgs(count)
          val p2: zio.IO[ReadError[String], SomeConfig] =
            fromArgs(args)
              .map(config => config)

          val expected = (1 to count).toList
          assertM(p2.either)(isRight(equalTo(SomeConfig(expected))))
        }
      }
    )

  final case class SomeConfig(ints: List[Int])

  object SomeConfig {
    val descriptor: ConfigDescriptor[SomeConfig] =
      list("ints")(int).to[SomeConfig]
  }

  def renderArgs(count: Int): List[String] =
    (1 to count)
      .map(i => s"--ints=$i")
      .toList

  def fromArgs(args: List[String]) =
    ZConfig.fromCommandLineArgs(args, SomeConfig.descriptor, None, None).build.useNow.map(_.get[SomeConfig])

}
