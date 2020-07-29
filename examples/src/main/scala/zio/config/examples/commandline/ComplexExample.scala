package zio.config.examples.commandline

import zio.config._, ConfigDescriptor._

object ComplexExample extends App {
  val argss =
    "--conf -database.username=1 --conf -database.password=hi --conf.database.url=jdbc://xyz --conf -num_execs=10 --vault.username=3 --vault.password=10 --vault.something=11 --users 1 --users 2 --region 111,112"

  val source = ConfigSource.fromCommandLineArgs(
    argss.split(' ').toList,
    Some('.'),
    Some(',')
  )

  final case class UserPassword(k2: String, k3: String)

  object UserPassword {
    val desc = (string("username") |@| string("password"))(
      UserPassword.apply,
      UserPassword.unapply
    )
  }

  final case class DatabaseConfig(conf: UserPassword, url: String)

  object DatabaseConfig {
    val desc = nested("database") {
      (UserPassword.desc |@| string("url"))(
        DatabaseConfig.apply,
        DatabaseConfig.unapply
      )
    }
  }

  final case class VaultConfig(userPassword: UserPassword)

  object VaultConfig {
    val desc =
      nested("vault") {
        UserPassword.desc
      }(VaultConfig.apply, VaultConfig.unapply)
  }

  final case class SparkConfig(databaseConfig: DatabaseConfig, numberOfExecutors: Int)

  object SparkConfig {
    val desc = (DatabaseConfig.desc |@| int("num_execs"))(
      SparkConfig.apply,
      SparkConfig.unapply
    )
  }

  final case class AppConfig(sparkConfig: SparkConfig, vault: VaultConfig, users: List[String], region: List[String])

  object AppConfig {
    val desc: ConfigDescriptor[AppConfig] =
      (nested("conf") { SparkConfig.desc } |@| VaultConfig.desc |@| list(
        "users"
      )(string) |@| list("region")(string))(AppConfig.apply, AppConfig.unapply)
  }

  assert(
    read(AppConfig.desc from (source)) ==
      Right(
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
  )
}
