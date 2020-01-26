package zio.config.examples

import zio.config._
import ConfigDescriptor._
import zio.DefaultRuntime
import zio.config.ConfigDocs, ConfigDocs._, Details._
import zio.config.ConfigDocs.Path
import ConfigSource._

object DefaultValueExample extends App {
  final case class PgmConfig(a: String, b: Either[String, Int])

  val conf: ConfigDescriptor[String, String, PgmConfig] =
    (string("HELLO").default("xyz") |@|
      string("SOMETHING").orElseEither(int("PORT").default(1)))(PgmConfig.apply, PgmConfig.unapply)

  val pgmConfig = conf from ConfigSource.fromEnv(None)

  val runtime = new DefaultRuntime {}

  // read(pgmConf from ConfigSource.fromEnv) is equivalent to Config.fromEnv(pgmConf) except that it returns `Config[A]` in return
  // which you can pass down to the rest of the program
  val result = runtime.unsafeRun(read(pgmConfig))

  assert(result == PgmConfig("xyz", Right(1)))

  assert(
    generateDocs(pgmConfig) ==
      Both(
        Path(
          "HELLO",
          Descriptions(
            Sources(List(EmptySource, ConfigSource.SystemEnvironment)),
            List("value of type string", "default value: xyz")
          )
        ),
        OneOf(
          Path(
            "SOMETHING",
            Descriptions(Sources(List(EmptySource, ConfigSource.SystemEnvironment)), List("value of type string"))
          ),
          Path(
            "PORT",
            Descriptions(
              Sources(List(EmptySource, ConfigSource.SystemEnvironment)),
              List("value of type int", "default value: 1")
            )
          )
        )
      )
  )

  assert(
    generateDocsWithValue(pgmConfig, result) ==
      Right(
        Both(
          Path(
            "HELLO",
            DescriptionsWithValue(
              Some("xyz"),
              Sources(List(EmptySource, SystemEnvironment)),
              List("value of type string", "default value: xyz")
            )
          ),
          OneOf(
            Path(
              "SOMETHING",
              DescriptionsWithValue(None, Sources(List(EmptySource, SystemEnvironment)), List("value of type string"))
            ),
            Path(
              "PORT",
              DescriptionsWithValue(
                Some("1"),
                Sources(List(EmptySource, SystemEnvironment)),
                List("value of type int", "default value: 1")
              )
            )
          )
        )
      )
  )
}
