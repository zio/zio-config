package zio.config.examples.commandline

import zio.config._

import examples._
import ConfigDescriptor._

object CommandLineComplex extends App {
  val argss =
    "--conf -database.username=1 --conf -database.password=hi --conf.database.url=jdbc://xyz --conf -num_execs=10 --vault.username=3 --vault.password=10 --vault.something=11 --users 1 --users 2 --region 111,112"

  val source: ConfigSource = ConfigSource.fromCommandLineArgs(
    argss.split(' ').toList,
    Some('.'),
    Some(',')
  )

  final case class UserPassword(k2: String, k3: String)

  object UserPassword {
    val desc: ConfigDescriptor[UserPassword] = (string("username") zip string("password")).to[UserPassword]
  }

  final case class DatabaseConfig(conf: UserPassword, url: String)

  object DatabaseConfig {
    val desc: ConfigDescriptor[DatabaseConfig] = nested("database") {
      (UserPassword.desc zip string("url")).to[DatabaseConfig]
    }
  }

  final case class VaultConfig(userPassword: UserPassword)

  object VaultConfig {
    val desc: ConfigDescriptor[VaultConfig] =
      nested("vault") {
        UserPassword.desc
      }.to[VaultConfig]
  }

  final case class SparkConfig(databaseConfig: DatabaseConfig, numberOfExecutors: Int)

  object SparkConfig {
    val desc: ConfigDescriptor[SparkConfig] = (DatabaseConfig.desc zip int("num_execs")).to[SparkConfig]
  }

  final case class AppConfig(sparkConfig: SparkConfig, vault: VaultConfig, users: List[String], region: List[String])

  object AppConfig {
    val desc: ConfigDescriptor[AppConfig] =
      (nested("conf")(SparkConfig.desc) zip VaultConfig.desc zip list(
        "users"
      )(string) zip list("region")(string)).to[AppConfig]
  }

  assert(
    read(AppConfig.desc from (source)) equalM
      AppConfig(
        SparkConfig(
          DatabaseConfig(UserPassword("1", "hi"), "jdbc://xyz"),
          10
        ),
        VaultConfig(UserPassword("3", "10")),
        List("1", "2"),
        List("111", "112")
      )
  )
}
