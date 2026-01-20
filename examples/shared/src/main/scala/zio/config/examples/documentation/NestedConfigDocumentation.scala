package zio.config.examples.documentation

import zio._
import zio.config._

final case class Credentials(user: String, password: String)
object Credentials {
  val config: Config[Credentials] = {
    Config.string("USERNAME") ?? "Example: ZioUser" zip
      Config.string("PASSWORD") ?? "Example: ZioPass"
  }.to[Credentials] ?? "Credentials"
}
final case class Database(port: Int, url: String)
object Database    {
  val config = {
    Config.int("PORT") ?? "Example: 8088" zip Config.string("URL") ?? "Example: abc.com"
  }.to[Database] ?? "Database"
}
final case class AppConfig(secret: Option[String], credentials: Credentials, database: Database)

object AppConfig {
  def config: Config[AppConfig] = {
    Config.string("SECRET").optional ?? ("Application secret") zip
      Credentials.config.nested("CREDENTIALS") zip
      Database.config.nested("DATABASE")
  }.to[AppConfig]
}

object NestedConfigDocumentation extends ZIOAppDefault {
  def run =
    ZIO.debug("Auto-generated documentation of AppConfig:") *>
      ZIO.debug(generateDocs(AppConfig.config).toTable.toGithubFlavouredMarkdown)
}
