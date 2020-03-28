package zio.config.typesafe

import zio.config._, Config._
import java.io.File
import zio.ZIO
import com.typesafe.config.ConfigFactory
import zio.{ system, Tagged, ZEnv, ZLayer }

object TypesafeConfig {

  def fromDefaultLoader[A](
    configDescriptor: ConfigDescriptor[String, String, A]
  )(implicit tagged: Tagged[Config.Service[A]]): ZLayer.NoDeps[Throwable, Config[A]] =
    fromHocon(ConfigFactory.load.resolve, configDescriptor)

  def fromHoconFile[A](
    configDescriptor: ConfigDescriptor[String, String, A],
    file: File
  )(implicit tagged: Tagged[Config.Service[A]]): ZLayer.NoDeps[Throwable, Config[A]] =
    fromHocon(ConfigFactory.parseFile(file).resolve, configDescriptor)

  def fromHoconString[A](
    str: String,
    configDescriptor: ConfigDescriptor[String, String, A]
  )(implicit tagged: Tagged[Config.Service[A]]): ZLayer.NoDeps[Throwable, Config[A]] =
    fromHocon(ConfigFactory.parseString(str).resolve, configDescriptor)

  def fromHocon[A](
    f: => com.typesafe.config.Config,
    configDescriptor: ConfigDescriptor[String, String, A]
  )(implicit tagged: Tagged[Config.Service[A]]): ZLayer.NoDeps[Throwable, Config[A]] =
    ZLayer.fromEffect(
      for {
        conf <- ZIO.effect(f)
        ln   <- system.lineSeparator.provideLayer(ZEnv.live)
        config <- makeM(TypeSafeConfigSource.hocon(Left(conf)), configDescriptor)
                   .mapError(r => new RuntimeException(s"${ln}${r.mkString(ln)}"))
      } yield config
    )
}
