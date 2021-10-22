package zio.config.examples

import zio.{Has, System, ZIO}
import zio.config._

import ConfigDescriptor._

object DefaultValueExample extends App {
  final case class PgmConfig(a: String, b: Either[String, Int])

  val conf: ConfigDescriptor[PgmConfig] =
    (string("HELLO").default("xyz") |@|
      string("SOMETHING").orElseEither(int("PORT").default(1)))(
      PgmConfig.apply,
      PgmConfig.unapply
    )

  val pgmConfig: ZIO[Has[System], ReadError[String], ConfigDescriptor[PgmConfig]] =
    ConfigSource.fromSystemEnv.map(source => conf from source)

  val runtime = zio.Runtime.default

  val confEx: ConfigDescriptor[PgmConfig] = runtime.unsafeRun(pgmConfig)

  // read(pgmConf from ConfigSource.fromEnv) is equivalent to Config.fromEnv(pgmConf) except that it returns `Config[A]` in return
  // which you can pass down to the rest of the program
  val expected: PgmConfig                          = PgmConfig("xyz", Right(1))
  val result: Either[ReadError[String], PgmConfig] = read(confEx)

  assert(result == Right(PgmConfig("xyz", Right(1))))

  assert(
    generateDocs(confEx).toTable.toGithubFlavouredMarkdown ==
      s"""
         |## Configuration Details
         |
         |
         ||FieldName|Format                     |Description|Sources|
         ||---      |---                        |---        |---    |
         ||         |[all-of](fielddescriptions)|           |       |
         |
         |### Field Descriptions
         |
         ||FieldName|Format                           |Description                             |Sources           |
         ||---      |---                              |---                                     |---               |
         ||HELLO    |primitive                        |value of type string, default value: xyz|system environment|
         ||         |[any-one-of](fielddescriptions-1)|                                        |                  |
         |
         |### Field Descriptions
         |
         ||FieldName|Format   |Description                        |Sources           |
         ||---      |---      |---                                |---               |
         ||SOMETHING|primitive|value of type string               |system environment|
         ||PORT     |primitive|value of type int, default value: 1|system environment|
         |""".stripMargin
  )
}
