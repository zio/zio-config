package zio.config.examples.commandline

import zio.config.ConfigDescriptor.{ list, nested, string }
import zio.config.{ read, ConfigDescriptor, ConfigSource }

object CommandLineArgs extends App {
  val argss =
    "--k --l --z --x -database.username=1 --k --l --z --x.database.password=hi --k --l --z --x        -database.url=jdbc://xyz --vault -username=3 --vault -password=10 --vault -something=11 --users 100 --region 111,122"

  val source = ConfigSource.fromCommandLineArgs(argss.split(' '), Some('.'), Some(','))

  final case class UserPassword(k2: String, k3: String)

  object UserPassword {
    val desc = (string("username") |@| string("password"))(UserPassword.apply, UserPassword.unapply)
  }

  final case class DatabaseConfig(conf: UserPassword, url: String)

  object DatabaseConfig {
    val desc = nested("database") {
      (UserPassword.desc |@| string("url"))(DatabaseConfig.apply, DatabaseConfig.unapply)
    }
  }

  final case class VaultConfig(userPassword: UserPassword)

  object VaultConfig {
    val desc =
      nested("vault") {
        UserPassword.desc
      }(VaultConfig.apply, VaultConfig.unapply)
  }

  final case class AppConfig(databaseConfig: DatabaseConfig, vault: VaultConfig, users: String, region: List[String])

  object AppConfig {
    val desc: ConfigDescriptor[String, String, AppConfig] =
      (nested("k") { nested("l") { nested("z") { nested("x") { DatabaseConfig.desc } } } } |@| VaultConfig.desc |@| string(
        "users"
      ) |@| list(
        string("region")
      ))(
        AppConfig.apply,
        AppConfig.unapply
      )
  }

  assert(
    read(AppConfig.desc from (source)) ==
      Right(
        AppConfig(
          DatabaseConfig(UserPassword("1", "hi"), "jdbc://xyz"),
          VaultConfig(UserPassword("3", "10")),
          "100",
          List("111", "122")
        )
      )
  )
}
