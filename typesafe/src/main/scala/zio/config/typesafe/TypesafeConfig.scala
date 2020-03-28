package zio.config.typesafe

import zio.config._, Config._
import java.io.File
import zio.ZIO
import com.typesafe.config.ConfigFactory
import zio.system.System.Live.system.lineSeparator
import zio.Task

object TypesafeConfig {
  def fromHoconFile[A](configDescriptor: ConfigDescriptor[String, String, A], file: File): Task[Config[A]] =
    getService(ConfigFactory.parseFile(file).resolve, configDescriptor)

  def fromHoconString[A](str: String, configDescriptor: ConfigDescriptor[String, String, A]): Task[Config[A]] =
    getService(ConfigFactory.parseString(str).resolve, configDescriptor)

  def getService[A](
    f: => com.typesafe.config.Config,
    configDescriptor: ConfigDescriptor[String, String, A]
  ): Task[Config[A]] =
    for {
      conf <- ZIO.effect(f)
      configSource <- ZIO
                       .fromEither(TypeSafeConfigSource.fromTypesafeConfig(conf))
                       .mapError(error => new RuntimeException(error): Throwable)

      service <- lineSeparator.flatMap(
                  ln => make(configSource, configDescriptor).mapError(r => new RuntimeException(s"${ln}${r}"))
                )

    } yield service
}
