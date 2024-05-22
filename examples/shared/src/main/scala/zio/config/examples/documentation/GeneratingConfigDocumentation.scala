package zio.config.examples.documentation

import zio._
import zio.config._

case class MyConfig(ldap: String, port: Int, dburl: String)
object MyConfig {
  val config = {
    Config.string("LDAP") ?? "Related to auth" zip
      Config.int("PORT") ?? "Database port" zip
      Config.string("DB_URL") ?? "URL of database"
  }.to[MyConfig]
}

object GeneratingConfigDocumentation extends ZIOAppDefault {
  def run =
    ZIO.debug("auto-generated documentation of MyConfig:") *>
      ZIO.debug(generateDocs(MyConfig.config).toTable.toGithubFlavouredMarkdown)
}
