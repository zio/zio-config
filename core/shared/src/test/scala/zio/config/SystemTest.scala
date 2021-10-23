package zio.config

import zio.config.ConfigDescriptor._
import zio.test.Assertion._
import zio.test.environment.{TestEnvironment, TestSystem}
import zio.test.{DefaultRunnableSpec, _}
import zio.{Has, Random, ZIO}

object SystemTest extends DefaultRunnableSpec {

  def spec: Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("Configuration from system")(
      test("from system properties") {
        check(genSomeConfig, genDelimiter) { (config, delimiter) =>
          val result = for {
            _ <- setSystemProperties(config, delimiter)
            p <- ZIO
                   .environment[Has[SomeConfig]]
                   .provideLayer(ZConfig.fromSystemProperties(SomeConfig.descriptor, Some(delimiter)))
          } yield p.get

          assertM(result.either)(isRight(equalTo(config)))
        }
      },
      test("from system environment") {
        val config = SomeConfig(100, "ABC")
        val result = fromSystemEnvResult(
          keyDelimiter = '_',
          sysEnv = Map("SYSTEMPROPERTIESTEST_SIZE" -> "100", "SYSTEMPROPERTIESTEST_DESCRIPTION" -> "ABC")
        )

        assertM(result.either)(isRight(equalTo(config)))
      },
      test("invalid system environment delimiter") {
        val keyDelimiter = '.'
        val result       = fromSystemEnvResult(keyDelimiter = keyDelimiter)

        assertM(result.either)(
          isLeft(
            equalTo(
              ReadError.SourceError(
                message = s"Invalid system key delimiter: $keyDelimiter",
                annotations = Set.empty
              )
            )
          )
        )
      }
    )
  import zio.test.environment.TestSystem._

  private def fromSystemEnvResult(keyDelimiter: Char, sysEnv: Map[String, String] = Map.empty) = {
    val sysEnvLayer    = TestSystem.live(Data(envs = sysEnv))
    val configEnvLayer = sysEnvLayer >>> ZConfig.fromSystemEnv(SomeConfig.descriptor, Some(keyDelimiter))

    ZIO.environment[Has[SomeConfig]].provideLayer(configEnvLayer).map(_.get)
  }

  final case class SomeConfig(size: Int, description: String)

  object SomeConfig {
    val descriptor: ConfigDescriptor[SomeConfig] =
      nested("SYSTEMPROPERTIESTEST")(
        (int("SIZE") |@| string("DESCRIPTION")).to[SomeConfig]
      )
  }

  def genSomeConfig: Gen[Has[Random] with Has[Sized], SomeConfig] =
    for {
      size <- Gen.int
      desc <- Gen.string
    } yield SomeConfig(size, desc)

  def genDelimiter: Gen[Has[Random], Char]       = Gen.elements('.', '_', '-', ':')
  def genSystemDelimiter: Gen[Has[Random], Char] = Gen.elements('_')

  def setSystemProperties(config: SomeConfig, delimiter: Char): ZIO[Has[TestSystem], Nothing, Unit] =
    for {
      _ <- TestSystem.putProperty(s"SYSTEMPROPERTIESTEST${delimiter}SIZE", config.size.toString)
      _ <- TestSystem.putProperty(s"SYSTEMPROPERTIESTEST${delimiter}DESCRIPTION", config.description)
    } yield ()

}
