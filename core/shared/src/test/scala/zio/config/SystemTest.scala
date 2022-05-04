package zio.config

import zio.config.ConfigDescriptor._
import zio.test.Assertion._
import zio.test._
import zio.{ZIO, _}

object SystemTest extends ZIOSpecDefault {
  def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Configuration from system")(
      test("from system properties") {
        check(genSomeConfig, genDelimiter) { (config, delimiter) =>
          val result = for {
            _ <- setSystemProperties(config, delimiter)
            p <- ZIO
                   .environment[SomeConfig]
                   .provideLayer(ZConfig.fromSystemProperties(SomeConfig.descriptor, Some(delimiter)))
          } yield p.get

          assertZIO(result.either)(isRight(equalTo(config)))
        }
      },
      test("from system environment") {
        val config = SomeConfig(100, "ABC")
        val result = fromSystemEnvResult(
          keyDelimiter = '_',
          sysEnv = Map("SYSTEMPROPERTIESTEST_SIZE" -> "100", "SYSTEMPROPERTIESTEST_DESCRIPTION" -> "ABC")
        )

        assertZIO(result.either)(isRight(equalTo(config)))
      },
      test("invalid system environment delimiter") {
        val keyDelimiter = '.'
        val result       = fromSystemEnvResult(keyDelimiter = keyDelimiter)
        assertZIO(result.sandbox.mapError(_.isDie).either)(isLeft(equalTo(true)))
      }
    )

  import zio.test.TestSystem._

  private def fromSystemEnvResult(keyDelimiter: Char, sysEnv: Map[String, String] = Map.empty) = {
    val sysEnvLayer    = TestSystem.live(Data(envs = sysEnv))
    val configEnvLayer = sysEnvLayer >>> ZConfig.fromSystemEnv(SomeConfig.descriptor, Some(keyDelimiter))

    ZIO.environment[SomeConfig].provideLayer(configEnvLayer).map(_.get)
  }

  final case class SomeConfig(size: Int, description: String)

  object SomeConfig {
    val descriptor: ConfigDescriptor[SomeConfig] =
      nested("SYSTEMPROPERTIESTEST")(
        (int("SIZE") zip string("DESCRIPTION")).to[SomeConfig]
      )
  }

  def genSomeConfig: Gen[Sized, SomeConfig] =
    for {
      size <- Gen.int
      desc <- Gen.string
    } yield SomeConfig(size, desc)

  def genDelimiter: Gen[Any, Char]       = Gen.elements('.', '_', '-', ':')
  def genSystemDelimiter: Gen[Any, Char] = Gen.elements('_')

  def setSystemProperties(config: SomeConfig, delimiter: Char): ZIO[Any, Nothing, Unit] =
    for {
      _ <- TestSystem.putProperty(s"SYSTEMPROPERTIESTEST${delimiter}SIZE", config.size.toString)
      _ <- TestSystem.putProperty(s"SYSTEMPROPERTIESTEST${delimiter}DESCRIPTION", config.description)
    } yield ()

}
