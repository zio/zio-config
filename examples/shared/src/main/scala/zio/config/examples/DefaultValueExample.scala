package zio.config.examples

import zio.Config
import zio.config.syntax._

object DefaultValueExample extends App {
  final case class PgmConfig(a: String, b: String)

  val conf: Config[PgmConfig] =
    Config.string("HELLO").withDefault("xyz").zip(Config.string("SOMETHING")).to[PgmConfig]

  val runtime = zio.Runtime.default

  // read(pgmConf from ConfigSource.fromEnv) is equivalent to Config.fromEnv(pgmConf) except that it returns `Config[A]` in return
  // which you can pass down to the rest of the program

  println(zio.config.generateDocs(conf).toTable.toGithubFlavouredMarkdown)

  /**
   *  ## Configuration Details
   *  |FieldName|Format                     |Description|Sources|
   *  |---      |---                        |---        |---    |
   *  |         |[all-of](fielddescriptions)|           |       |
   *  ### Field Descriptions
   *  |FieldName|Format                           |Description|Sources|
   *  |---      |---                              |---        |---    |
   *  |         |[any-one-of](fielddescriptions-1)|           |       |
   *  |SOMETHING|recursion                        |           |       |
   *  ### Field Descriptions
   *  |FieldName|Format   |Description|Sources|
   *  |---      |---      |---        |---    |
   *  |HELLO    |recursion|           |       |
   *  |         |recursion|           |       |
   */

}
