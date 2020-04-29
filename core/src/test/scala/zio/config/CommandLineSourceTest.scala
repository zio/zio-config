package zio.config

import zio.config.ReadError.{ ConversionError, Step }
import zio.config.testsupport.MapConfigTestSupport.AppConfig.descriptor
import zio.config.testsupport.MapConfigTestSupport.{ genAppConfig, stringNWithInjector, AppConfig }
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test.{ DefaultRunnableSpec, _ }
import zio.{ IO, ZIO }

object CommandLineSourceTest extends DefaultRunnableSpec {
  def spec: Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("Configuration from command-line-style arguments")(
      testM("Configuration from arguments roundtrip separate args --key value") {
        checkM(genAppConfig()) { appConfig =>
          val p2: zio.IO[ReadError[String], AppConfig] =
            for {
              args   <- toSeparateArgs(AppConfig.descriptor, appConfig)
              reread <- fromArgs(args)
            } yield reread.get

          assertM(p2.either)(isRight(equalTo(appConfig)))
        }
      },
      testM("Configuration from arguments roundtrip single arg --key=value") {
        checkM(genAppConfig()) { appConfig =>
          val p2: zio.IO[ReadError[String], AppConfig] =
            for {
              args   <- toSingleArg(AppConfig.descriptor, appConfig)
              reread <- fromArgs(args)
            } yield reread.get

          assertM(p2.either)(isRight(equalTo(appConfig)))
        }
      },
      testM("Configuration from arguments roundtrip single arg --key=value multiple values take the head") {
        checkM(genAppConfig()) { appConfig =>
          val p2: zio.IO[ReadError[String], AppConfig] =
            for {
              args   <- toMultiSingleArg(AppConfig.descriptor, appConfig)
              reread <- fromArgs(args)
            } yield reread.get

          assertM(p2.either)(isRight(equalTo(appConfig)))
        }
      },
      testM("Configuration from arguments roundtrip singe arg --key-value where value contains = char") {
        checkM(genAppConfig(stringNWithInjector(1, 15, "="))) { appConfig =>
          val p2: zio.IO[ReadError[String], AppConfig] =
            for {
              args   <- toSingleArg(AppConfig.descriptor, appConfig)
              reread <- fromArgs(args)
            } yield reread.get

          assertM(p2.either)(isRight(equalTo(appConfig)))
        }
      }
    )

  def fromArgs(args: List[String]): ZIO[Any, ReadError[String], Config[AppConfig]] =
    ZIO.environment.provideLayer(Config.fromCommandLineArgs(args, descriptor, Some('_'), None))

  def toSeparateArgs[A](
    descriptor: ConfigDescriptor[A],
    a: A
  ): ZIO[Any, ReadError[String], List[String]] =
    IO.fromEither(write(descriptor, a))
      .bimap(
        s => ConversionError[String](List(Step.Index(0)), s),
        propertyTree =>
          propertyTree.flatten.toList.flatMap { t: (Vector[String], ::[String]) =>
            List(s"--${t._1.mkString("_")}", t._2.mkString)
          }
      )

  def toSingleArg[A](descriptor: ConfigDescriptor[A], a: A): ZIO[Any, ReadError[String], List[String]] =
    IO.fromEither(write(descriptor, a))
      .bimap(
        s => ConversionError[String](List(Step.Index(0)), s),
        propertyTree =>
          propertyTree.flatten.toList.flatMap { t: (Vector[String], ::[String]) =>
            List(s"-${t._1.mkString("_")}=${t._2.mkString}")
          }
      )

  def toMultiSingleArg[A](
    descriptor: ConfigDescriptor[A],
    a: A
  ): ZIO[Any, ReadError[String], List[String]] =
    IO.fromEither(write(descriptor, a))
      .bimap(
        s => ConversionError[String](List(Step.Index(0)), s),
        propertyTree =>
          propertyTree.flatten.toList.flatMap { t: (Vector[String], ::[String]) =>
            List(s"--${t._1.mkString("_")}=${t._2.mkString}") ++
              List(s"--${t._1.mkString("_")}=${t._2.mkString}") // repeated with different value (should be ignored)
          }
      )

}
