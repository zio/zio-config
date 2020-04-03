package zio.config

import zio.config.ReadError.ConversionError
import zio.config.testsupport.MapConfigTestSupport.AppConfig.descriptor
import zio.config.testsupport.MapConfigTestSupport.{ genAppConfig, AppConfig }
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test.{ DefaultRunnableSpec, _ }
import zio.{ IO, ZIO }

object ArgsConfigTest extends DefaultRunnableSpec {
  def spec: Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("Configuration from command-line-style arguments")(
      testM("Configuration from arguments roundtrip separate args --key value") {
        checkM(genAppConfig) { appConfig =>
          val p2: zio.IO[ReadError[String], AppConfig] =
            for {
              args   <- toArgs(AppConfig.descriptor, appConfig, true)
              reread <- fromArgs(args)
            } yield reread.get

          assertM(p2.either)(isRight(equalTo(appConfig)))
        }
      }
      /*testM("Configuration from arguments roundtrip single arg --key=value") {
        checkM(genAppConfig) { appConfig =>
          val p2: zio.IO[ReadError[String], AppConfig] =
            for {
              args   <- toArgs(AppConfig.descriptor, appConfig, false)
              reread <- fromArgs(args)
            } yield reread.get

          assertM(p2.either)(isRight(equalTo(appConfig)))
        }
      }*/
    )

  def fromArgs(args: List[String]): ZIO[Any, ReadError[String], Config[AppConfig]] =
    ZIO.environment.provideLayer(Config.fromArgs(args, descriptor, Some('_'), None))

  def toArgs[A](
    descriptor: ConfigDescriptor[String, String, A],
    a: A,
    separateArgs: Boolean
  ): ZIO[Any, ReadError[String], List[String]] =
    IO.fromEither(write(descriptor, a))
      .bimap(
        s => ConversionError[String](Vector(Left(0)), s),
        propertyTreeArgs(_, separateArgs)
      )

  def propertyTreeArgs(propertyTree: PropertyTree[String, String], separateArgs: Boolean): List[String] =
    propertyTree.flatten.toList.flatMap { t: (Vector[String], ::[String]) =>
      if (separateArgs: Boolean) List(s"--${t._1.mkString("_")}", t._2.mkString)
      else List(s"--${t._1.mkString("_")}=${t._2.mkString}")
    }

}
