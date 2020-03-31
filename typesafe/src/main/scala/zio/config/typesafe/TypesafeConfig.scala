package zio.config.typesafe

import java.io.File

import com.typesafe.config.ConfigFactory
import zio.config._
import zio.{ Layer, Tagged, ZIO }

object TypesafeConfig {

  def fromDefaultLoader[A](
    configDescriptor: ConfigDescriptor[String, String, A]
  )(implicit tagged: Tagged[A]): Layer[Throwable, Config[A]] =
    fromTypesafeConfig(ConfigFactory.load.resolve, configDescriptor)

  def fromHoconFile[A](
    configDescriptor: ConfigDescriptor[String, String, A],
    file: File
  )(implicit tagged: Tagged[A]): Layer[Throwable, Config[A]] =
    fromTypesafeConfig(ConfigFactory.parseFile(file).resolve, configDescriptor)

  def fromHoconString[A](
    str: String,
    configDescriptor: ConfigDescriptor[String, String, A]
  )(implicit tagged: Tagged[A]): Layer[Throwable, Config[A]] =
    fromTypesafeConfig(ConfigFactory.parseString(str).resolve, configDescriptor)

  def fromTypesafeConfig[A](
    f: => com.typesafe.config.Config,
    configDescriptor: ConfigDescriptor[String, String, A]
  )(implicit tagged: Tagged[A]): Layer[Throwable, Config[A]] =
    Config.fromConfigDescriptorM(
      for {
        conf <- ZIO.effect(f)
        source <- ZIO
                   .fromEither(TypeSafeConfigSource.fromTypesafeConfig(conf))
                   .mapError(error => new RuntimeException(error))
      } yield configDescriptor from source
    )
}
