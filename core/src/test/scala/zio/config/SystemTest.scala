package zio.config

import zio.ZIO
import zio.config.ConfigDescriptor.{ int, nested, string }
import zio.random.Random
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test.{ DefaultRunnableSpec, _ }

object SystemTest extends DefaultRunnableSpec {

  def spec: Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("Configuration from system")(
      testM("from system properties") {
        checkM(genSomeConfig, genDelimiter) { (config, delimiter) =>
          val result = for {
            _ <- setSystemProperties(config, delimiter)
            p <- ZIO.environment.provideLayer(Config.fromSystemProperties(SomeConfig.descriptor, Some(delimiter)))
            _ <- clearSystemProperties(delimiter)
          } yield p.get

          assertM(result.either)(isRight(equalTo(config)))
        }
      }
    )

  final case class SomeConfig(size: Int, description: String)

  object SomeConfig {
    val descriptor: ConfigDescriptor[String, String, SomeConfig] =
      nested("SYSTEMPROPERTIESTEST")(
        (int("SIZE") |@| string("DESCRIPTION"))(SomeConfig.apply, SomeConfig.unapply)
      )
  }

  def genSomeConfig: Gen[Random with Sized, SomeConfig] =
    for {
      size <- Gen.anyInt
      desc <- Gen.anyString
    } yield SomeConfig(size, desc)

  def genDelimiter: Gen[Random, Char] = Gen.elements('.', '_', '-', ':')

  def setSystemProperties(config: SomeConfig, delimiter: Char) = ZIO.succeed {
    java.lang.System.setProperty(s"SYSTEMPROPERTIESTEST${delimiter}SIZE", config.size.toString)
    java.lang.System.setProperty(s"SYSTEMPROPERTIESTEST${delimiter}DESCRIPTION", config.description)
  }

  def clearSystemProperties(delimiter: Char) = ZIO.succeed {
    java.lang.System.clearProperty(s"SYSTEMPROPERTIESTEST${delimiter}SIZE")
    java.lang.System.clearProperty(s"SYSTEMPROPERTIESTEST${delimiter}DESCRIPTION")
  }

}
