package zio.config.examples

import zio.ZIO
import zio.config._

import ConfigDescriptor._

object DefaultValueExample extends App {
  final case class PgmConfig(a: String, b: Either[String, Int])

  val conf: ConfigDescriptor[PgmConfig] =
    (string("HELLO").default("xyz") zip
      string("SOMETHING").orElseEither(int("PORT").default(1))).to[PgmConfig]

  val pgmConfig: ConfigDescriptor[PgmConfig] =
    conf from ConfigSource.fromSystemEnv()

  val runtime = zio.Runtime.default

  // read(pgmConf from ConfigSource.fromEnv) is equivalent to Config.fromEnv(pgmConf) except that it returns `Config[A]` in return
  // which you can pass down to the rest of the program
  val expected: PgmConfig                            = PgmConfig("xyz", Right(1))
  val result: ZIO[Any, ReadError[String], PgmConfig] = read(pgmConfig)

  assert(result equalM PgmConfig("xyz", Right(1)))

  assert(
    generateDocs(pgmConfig).toTable.toGithubFlavouredMarkdown ==
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
