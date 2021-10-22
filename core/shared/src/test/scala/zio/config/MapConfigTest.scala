package zio.config

import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment
import zio.{Has, IO, ZIO}

import ReadError._
import testsupport.MapConfigTestSupport.AppConfig.descriptor

object MapConfigTest extends DefaultRunnableSpec {
  import zio.config.testsupport.MapConfigTestSupport._

  def spec: Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("Configuration from Map")(
      test("Configuration from Map roundtrip") {
        check(genAppConfig()) { appConfig =>
          val p2: zio.IO[ReadError[String], AppConfig] =
            for {
              args   <- toMap(AppConfig.descriptor, appConfig)
              reread <- fromMap(args)
            } yield reread.get

          assertM(p2.either)(isRight(equalTo(appConfig)))
        }
      }
    )

  def fromMap(args: Map[String, String]): ZIO[Any, ReadError[String], Has[AppConfig]] =
    ZIO.environment.provideLayer(ZConfig.fromMap(args, descriptor, "WTL", Some('_'), None))

  def toMap[A](
    descriptor: ConfigDescriptor[A],
    a: A
  ): ZIO[Any, ReadError[String], Map[String, String]] =
    IO.fromEither(write(descriptor, a))
      .mapBoth(
        s => ConversionError[String](List(Step.Index(0)), s),
        propertyTreeArgs
      )
      .map(tuples => Map(tuples: _*))

  def propertyTreeArgs(propertyTree: PropertyTree[String, String]): List[(String, String)] =
    propertyTree.flatten.toList.map { (t: (Vector[String], ::[String])) =>
      (s"${t._1.mkString("_")}", s"${t._2.mkString}")
    }

}
