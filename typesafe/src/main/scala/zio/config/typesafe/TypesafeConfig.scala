package zio.config.typesafe

import zio.config._, Config._
import java.io.File
import zio.ZIO
import com.typesafe.config.ConfigFactory
import zio.system.System.Live.system.lineSeparator
import zio.Task

object TypesafeConfig {
  def fromHoconFile[A](configDescriptor: ConfigDescriptor[String, String, A], file: File): Task[Config[A]] =
    fromHocon(ConfigFactory.parseFile(file).resolve, configDescriptor)

  def fromHoconString[A](str: String, configDescriptor: ConfigDescriptor[String, String, A]): Task[Config[A]] =
    fromHocon(ConfigFactory.parseString(str).resolve, configDescriptor)

  def fromHocon[A](
    f: => com.typesafe.config.Config,
    configDescriptor: ConfigDescriptor[String, String, A]
  ): Task[Config[A]] =
    ZIO
      .effect(f)
      .flatMap(
        conf =>
          lineSeparator.flatMap(
            ln =>
              make(TypeSafeConfigSource.hocon(Left(conf)), configDescriptor)
                .mapError(r => new RuntimeException(s"${ln}${r}"))
          )
      )
}
