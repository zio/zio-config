package zio.config.examples

import zio.config._
import Config._
import zio.DefaultRuntime
import zio.config.actions.ConfigDocs
import zio.config.actions.ConfigDocs.KeyDescription

object DefaultValueExample extends App {
  final case class PgmConfig(a: String, b: Int)

  val pgmConf: ConfigDescriptor[PgmConfig] =
    (string("HELLO").default("xyz") |@| int("PORT").default(1))(PgmConfig.apply, PgmConfig.unapply)

  val runtime = new DefaultRuntime {}

  val result = runtime.unsafeRun(Config.fromEnv(pgmConf).flatMap(t => config[PgmConfig].provide(t)))

  assert(result == PgmConfig("xyz", 1))

  assert(
    docs(pgmConf) ==
      ConfigDocs(
        List(
          KeyDescription("HELLO", List("value of type string", "default value: xyz")),
          KeyDescription("PORT", List("value of type int", "default value: 1"))
        ),
        None
      )
  )
}
