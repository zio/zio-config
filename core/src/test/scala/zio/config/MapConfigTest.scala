package zio.config

import zio.config.ReadError.{ ConversionError, Step }
import zio.config.testsupport.MapConfigTestSupport.AppConfig.descriptor
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment
import zio.{ IO, ZIO }

object MapConfigTest extends DefaultRunnableSpec {
  import zio.config.testsupport.MapConfigTestSupport._

  def spec: Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("Configuration from Map")(
      testM("Configuration from Map roundtrip") {
        checkM(genAppConfig()) { appConfig =>
          val p2: zio.IO[ReadError[String], AppConfig] =
            for {
              args   <- toMap(AppConfig.descriptor, appConfig)
              reread <- fromMap(args)
            } yield reread.get

          assertM(p2.either)(isRight(equalTo(appConfig)))
        }
      }
    )

  def fromMap(args: Map[String, String]): ZIO[Any, ReadError[String], Config[AppConfig]] =
    ZIO.environment.provideLayer(Config.fromMap(args, descriptor, "WTL", Some('_'), None))

  def toMap[A](
    descriptor: ConfigDescriptor[A],
    a: A
  ): ZIO[Any, ReadError[String], Map[String, String]] =
    IO.fromEither(write(descriptor, a))
      .bimap(
        s => ConversionError[String](List(Step.Index(0)), s),
        propertyTreeArgs
      )
      .map(tuples => Map(tuples: _*))

  def propertyTreeArgs(propertyTree: PropertyTree[String, String]): List[(String, String)] =
    propertyTree.flatten.toList.map { t: (Vector[String], ::[String]) =>
      (s"${t._1.mkString("_")}", s"${t._2.mkString}")
    }

}
