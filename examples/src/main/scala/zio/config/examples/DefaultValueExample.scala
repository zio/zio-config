package zio.config.examples

import zio.config._, ConfigDescriptor._
import zio.config.ConfigDocs._
import zio.config.ConfigSource, ConfigSource._
import zio.config.ConfigDocs

object DefaultValueExample extends App {
  final case class PgmConfig(a: String, b: Either[String, Int])

  val conf: ConfigDescriptor[PgmConfig] =
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
      ConfigDocs.Zip(
        ConfigDocs.Nested(
          "HELLO",
          Leaf(
            Set(ConfigSourceName(ConfigSource.SystemEnvironment)),
            List("value of type string", "default value: xyz")
          )
        ),
        ConfigDocs.OrElse(
          ConfigDocs.Nested(
            "SOMETHING",
            Leaf(Set(ConfigSourceName(ConfigSource.SystemEnvironment)), List("value of type string"))
          ),
          ConfigDocs.Nested(
            "PORT",
            Leaf(
              Set(ConfigSourceName(ConfigSource.SystemEnvironment)),
              List("value of type int", "default value: 1")
            )
          )
        )
      )
  )

  assert(
    generateReport(confEx, expected) ==
      Right(
        ConfigDocs.Zip(
          ConfigDocs.Nested(
            "HELLO",
            Leaf(
              Set(ConfigSourceName(SystemEnvironment)),
              List("value of type string", "default value: xyz"),
              Some("xyz")
            )
          ),
          ConfigDocs.OrElse(
            ConfigDocs.Nested(
              "SOMETHING",
              Leaf(Set(ConfigSourceName(SystemEnvironment)), List("value of type string"), None)
            ),
            ConfigDocs.Nested(
              "PORT",
              ConfigDocs.Leaf(
                Set(ConfigSourceName(SystemEnvironment)),
                List("value of type int", "default value: 1"),
                Some("1")
              )
            )
          )
        )
      )
  )
}
