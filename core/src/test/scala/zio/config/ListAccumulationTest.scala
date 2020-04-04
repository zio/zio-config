package zio.config

import zio.ZIO
import zio.config.ConfigDescriptor.{ int, list }
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test.{ DefaultRunnableSpec, _ }

object ListAccumulationTest extends DefaultRunnableSpec {

  def spec: Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("Configuration of a list from multiple entries")(
      testM("Using single arg --key=value style") {
        checkM(Gen.int(0, 10)) { count =>
          val args = renderArgs(count)
          val p2: zio.IO[ReadError[String], SomeConfig] =
            fromArgs(args)
              .map(config => config.get)

          val expected = (0 until count).toList
          assertM(p2.either)(isRight(equalTo(SomeConfig(expected))))
        }
      }
    )

  final case class SomeConfig(ints: List[Int])

  object SomeConfig {
    val descriptor: ConfigDescriptor[String, String, SomeConfig] =
      list(int("ints"))(SomeConfig.apply, SomeConfig.unapply)
  }

  def renderArgs(count: Int): List[String] =
    (0 until count)
      .map(i => s"--ints=$i")
      .toList

  def fromArgs(args: List[String]): ZIO[Any, ReadError[String], Config[SomeConfig]] =
    ZIO.environment.provideLayer(Config.fromArgs(args, SomeConfig.descriptor, None, None))

}
