package zio.config

import zio.ZIO
import zio.config.ConfigDescriptor._
import zio.test.Assertion._
import zio.test.{TestEnvironment, ZIOSpecDefault, _}

object ArgsListAccumulationTest extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment, Nothing] =
    suite("Configuration of a list from multiple entries")(
      test("Using single arg --key=value style") {
        check(Gen.int(1, 10)) { count =>
          val args                                      = renderArgs(count)
          val p2: zio.IO[ReadError[String], SomeConfig] =
            fromArgs(args)
              .map(config => config)

          val expected = (1 to count).toList // O is missing values.
          assertZIO(p2.either)(isRight(equalTo(SomeConfig(expected))))
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

  def fromArgs(args: List[String]): ZIO[Any, ReadError[String], SomeConfig] =
    ZIO.scoped(ZConfig.fromCommandLineArgs(args, SomeConfig.descriptor, None, None).build.map(_.get[SomeConfig]))

}
