package zio.config.examples

import zio.{ App, ZIO }
import zio.config._

object ReadConfig extends App {

  final case class Ldap(value: String)  extends AnyVal
  final case class DbUrl(value: String) extends AnyVal

  object DbUrl {
    def fromString(s: String): Either[ReadError, DbUrl] =
      Right(DbUrl(s))
  }

  case class Prod(ldap: Ldap, dburl: Either[Option[DbUrl], Option[String]])

  case class Hello(x: Ldap, prod: Prod)

  val ldap = string("LDAP").xmap(Ldap)(_.value)

  private val configXx: Config[Prod] =
    (ldap <*>
      opt(string("DB_URL").mapEither(DbUrl.fromString)(value => Right(value.value)))
        .or(opt(string("DURL"))))(Prod.apply, Prod.unapply)

  val configX: Config[Hello] = (ldap <*> configXx)(Hello.apply, Hello.unapply)

  private val helloConfig =
    string("hello").xmap(identity)(identity)

  private val config: Config[Either[Option[String], Option[Hello]]] =
    opt(helloConfig) or opt(configX)

  // In real, this comes from environment
  private val validConfig =
    Map(
      "LDAP" -> "v1",
      "x"    -> "hi",
      "DURL" -> "v2"
    )

  override def run(args: List[String]): ZIO[ReadConfig.Environment, Nothing, Int] =
    ZIO.accessM { _ =>
      val appLogic =
        for {
          _ <- readMapAndWriteAndReadIsEqual
          _ <- writeConfigAndReadAndWriteAndReadAreAllEqual
        } yield ()

      appLogic.fold(_ => 1, _ => 0)
    }

  val readMapAndWriteAndReadIsEqual =
    ZIO.accessM[Environment] { env =>
      for {
        _        <- ZIO.effectTotal(println("read and write and read returns the same value"))
        conf     <- read(config).run.provide(mapSource(validConfig))
        map      <- write(config).run.provide(conf._2)
        readConf <- read(config).run.provide(mapSource(map.allConfig))
        _        <- env.console.putStrLn(readConf._2.toString)
        _        <- env.console.putStrLn(conf._2.toString)
      } yield ()
    }

  val writeConfigAndReadAndWriteAndReadAreAllEqual = {
    val manual =
      Right(Some(Hello(Ldap("v1"), Prod(Ldap("v1"), Right(Some("v2"))))))

    ZIO.accessM[Environment] { env =>
      for {
        map   <- write(config).run.provide(manual)
        conf  <- read(config).run.provide(mapSource(map.allConfig))
        map2  <- write(config).run.provide(conf._2)
        conf2 <- read(config).run.provide(mapSource(map2.allConfig))
        _     <- env.console.putStrLn(manual.toString)
        _     <- env.console.putStrLn(conf._2.toString)
        _     <- env.console.putStrLn(conf2._2.toString)
        _     <- env.console.putStrLn(map.toString)
        _     <- env.console.putStrLn(map2.toString)
      } yield ()
    }
  }
}
