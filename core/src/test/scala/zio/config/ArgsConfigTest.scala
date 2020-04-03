package zio.config

import zio.config.ReadError.ConversionError
import zio.config.testsupport.MapConfigTestSupport.AppConfig.descriptor
import zio.config.testsupport.MapConfigTestSupport.{AppConfig, genAppConfig}
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test.{DefaultRunnableSpec, _}
import zio.{IO, ZIO}

object ArgsConfigTest extends DefaultRunnableSpec {
  def spec: Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("Configuration from command-line-style arguments")(
      testM("Configuration from arguments roundtrip") {
        checkM(genAppConfig) { appConfig =>
          val p2: zio.IO[ReadError[String], AppConfig] =
            for {
              args   <- toArgs(AppConfig.descriptor, appConfig)
              reread <- fromArgs(args)
            } yield reread.get

          assertM(p2.either)(isRight(equalTo(appConfig)))
        }
      }
    )

  def fromArgs(args: List[String]): ZIO[Any, ReadError[String], Config[AppConfig]] =
    ZIO.environment.provideLayer(Config.fromArgs(args, descriptor, Some('_'), None))

  def toArgs[A](
    descriptor: ConfigDescriptor[String, String, A],
    a: A
  ): ZIO[Any, ReadError[String], List[String]] =
    IO.fromEither(write(descriptor, a))
      .bimap(
        s => ConversionError[String](Vector(Left(0)), s),
        propertyTreeArgs
      )

  def propertyTreeArgs(propertyTree: PropertyTree[String, String]): List[String] =
    propertyTree.flatten.toList.map { t: (Vector[String], ::[String]) =>
      s"--${t._1.mkString("_")}=${t._2.mkString}"
    }

}
