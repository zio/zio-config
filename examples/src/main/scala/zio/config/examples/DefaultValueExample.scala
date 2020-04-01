package zio.config.examples

import zio.config._
import ConfigDescriptor._
import zio.config.ConfigDocs
import ConfigDocs._
import Details._
import zio.config.ConfigDocs.Path
import ConfigSource._

object DefaultValueExample extends App {
  final case class PgmConfig(a: String, b: Either[String, Int])

  val conf: ConfigDescriptor[String, String, PgmConfig] =
    (string("HELLO").default("xyz") |@|
      string("SOMETHING").orElseEither(int("PORT").default(1)))(PgmConfig.apply, PgmConfig.unapply)

  val pgmConfig = ConfigSource.fromSystemEnv.map(source => conf from source)

  val runtime = zio.Runtime.default

  val confEx = runtime.unsafeRun(pgmConfig)

  // read(pgmConf from ConfigSource.fromEnv) is equivalent to Config.fromEnv(pgmConf) except that it returns `Config[A]` in return
  // which you can pass down to the rest of the program
  val expected = PgmConfig("xyz", Right(1))
  val result   = read(confEx)

  assert(result == Right(PgmConfig("xyz", Right(1))))

  println(generateDocs(confEx))
  assert(
    generateDocs(confEx) ==
      Both(
        Path(
          "HELLO",
          Descriptions(
            Sources(List(ConfigSource.SystemEnvironment)),
            List("value of type string", "default value: xyz")
          )
        ),
        OneOf(
          Path(
            "SOMETHING",
            Descriptions(Sources(List(ConfigSource.SystemEnvironment)), List("value of type string"))
          ),
          Path(
            "PORT",
            Descriptions(
              Sources(List(ConfigSource.SystemEnvironment)),
              List("value of type int", "default value: 1")
            )
          )
        )
      )
  )

  assert(
    generateDocsWithValue(confEx, expected) ==
      Right(
        Both(
          Path(
            "HELLO",
            DescriptionsWithValue(
              Some("xyz"),
              Sources(List(SystemEnvironment)),
              List("value of type string", "default value: xyz")
            )
          ),
          OneOf(
            Path(
              "SOMETHING",
              DescriptionsWithValue(None, Sources(List(SystemEnvironment)), List("value of type string"))
            ),
            Path(
              "PORT",
              DescriptionsWithValue(
                Some("1"),
                Sources(List(SystemEnvironment)),
                List("value of type int", "default value: 1")
              )
            )
          )
        )
      )
  )
}
