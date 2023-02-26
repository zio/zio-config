package zio.config.yaml

import zio.config._
import zio.config.xml.experimental._
import zio.config.yaml.XmlProviderSpec.Configuration.{Aws, Database}
import zio.test.Assertion._
import zio.test._
import zio.{Config, ConfigProvider}

object XmlProviderSpec extends ZIOSpecDefault {

  final case class Configuration(aws: Aws, database: Database)

  object Configuration {
    val config: Config[Configuration] =
      Aws.config.nested("aws").zip(Database.config.nested("database")).to[Configuration].nested("config")

    final case class Aws(region: String, account: String)

    object Aws {
      val config: Config[Aws] = Config.string("region").zip(Config.string("account")).to[Aws]
    }
    final case class Database(port: Int, url: String)

    object Database {
      val config: Config[Database] = Config.int("port").zip(Config.string("url")).to[Database]
    }
  }

  def spec: Spec[Any, Config.Error] =
    suite("Xml config provider spec")(
      test("load simple xml") {

        val config =
          s"""
             |<config>
             |  <aws region="us-east" account="personal"></aws>
             |  <database port="123" url="some url"></database>
             |</config>
             |
             |""".stripMargin

        val parsed = ConfigProvider.fromYamlString(config).load(Configuration.config)

        assertZIO(parsed)(equalTo(Configuration(Aws("us-east", "personal"), Database(123, "some url"))))
      }
    )
}
