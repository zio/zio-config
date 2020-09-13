package zio.config

import zio.ZIO
import zio.config.ConfigDescriptor._
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test.{ DefaultRunnableSpec, _ }

object ArgsListAccumulationTest extends DefaultRunnableSpec {

  def spec: Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("Configuration of a list from multiple entries")(
      testM("Using single arg --key=value style") {
        checkM(Gen.int(1, 10)) { count =>
          val args = renderArgs(count)
          val p2: zio.IO[ReadError[String], SomeConfig] =
            fromArgs(args)
              .map(config => config.get)

          val expected = (1 to count).toList // O is missing values.
          assertM(p2.either)(isRight(equalTo(SomeConfig(expected))))
        }
      }
    )

  final case class SomeConfig(ints: List[Int])

  object SomeConfig {
    val descriptor: ConfigDescriptor[SomeConfig] =
      list("ints")(int)(SomeConfig.apply, SomeConfig.unapply)
  }

  def renderArgs(count: Int): List[String] =
    (1 to count)
      .map(i => s"--ints=$i")
      .toList

  def fromArgs(args: List[String]): ZIO[Any, ReadError[String], ZConfig[SomeConfig]] =
    ZIO.environment.provideLayer(ZConfig.fromCommandLineArgs(args, SomeConfig.descriptor, None, None))

}
