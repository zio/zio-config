package zio.config.examples

import zio.config.ConfigDescriptor._
import zio.config.ConfigDocs._
import zio.config.ConfigSource._
import zio.config._

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
        NestedPath(
          "HELLO",
          Leaf(
            Sources(List(ConfigSource.SystemEnvironment)),
            List("value of type string", "default value: xyz")
          )
        ),
        OneOf(
          NestedPath(
            "SOMETHING",
            Leaf(Sources(List(ConfigSource.SystemEnvironment)), List("value of type string"))
          ),
          NestedPath(
            "PORT",
            Leaf(
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
          NestedPath(
            "HELLO",
            Leaf(
              Sources(List(SystemEnvironment)),
              List("value of type string", "default value: xyz"),
              Some("xyz")
            )
          ),
          OneOf(
            NestedPath(
              "SOMETHING",
              Leaf(Sources(List(SystemEnvironment)), List("value of type string"), None)
            ),
            NestedPath(
              "PORT",
              Leaf(
                Sources(List(SystemEnvironment)),
                List("value of type int", "default value: 1"),
                Some("1")
              )
            )
          )
        )
      )
  )
}
