package zio.config.examples

import zio.config._
import ConfigDescriptor._
import zio.DefaultRuntime
import zio.config.actions.ConfigDocs, ConfigDocs._
import zio.config.actions.ConfigDocs.PathDetails

object DefaultValueExample extends App {
  final case class PgmConfig(a: String, b: Either[String, Int])

  val pgmConf: ConfigDescriptor[String, String, PgmConfig] =
    (string("HELLO").default("xyz") |@|
      string("SOMETHING").orElseEither(int("PORT").default(1)))(PgmConfig.apply, PgmConfig.unapply)

  val runtime = new DefaultRuntime {}

  // read(pgmConf from ConfigSource.fromEnv) is equivalent to Config.fromEnv(pgmConf) except that it returns `Config[A]` in return
  // which you can pass down to the rest of the program
  val result = runtime.unsafeRun(read(pgmConf from ConfigSource.fromEnv))

  assert(result == PgmConfig("xyz", Right(1)))

  assert(
    docs(pgmConf, Some(result)) ==
      And(
        PathDetails("HELLO", Some("xyz"), List("value of type string", "default value: xyz")),
        Or(
          PathDetails("SOMETHING", None, List("value of type string")),
          PathDetails("PORT", Some("1"), List("value of type int", "default value: 1"))
        )
      )
  )
}
