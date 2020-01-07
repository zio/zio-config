package zio.config.typesafe

import zio.config._, Config._
import java.io.File
import zio.ZIO
import com.typesafe.config.ConfigFactory
import zio.system.System.Live.system.lineSeparator
import zio.Task

object TypesafeConfig {
  def fromHocconFile[A](configDescriptor: ConfigDescriptor[String, String, A], file: File) =
    fromHoccon(ConfigFactory.parseFile(file).resolve, configDescriptor)

  def fromHocconString[A](str: String, configDescriptor: ConfigDescriptor[String, String, A]): Task[Config[A]] =
    fromHoccon(ConfigFactory.parseString(str).resolve, configDescriptor)

  def fromHoccon[A](
    f: => com.typesafe.config.Config,
    configDescriptor: ConfigDescriptor[String, String, A]
  ): Task[Config[A]] =
    ZIO
      .effect(f)
      .flatMap(
        conf =>
          lineSeparator.flatMap(
            ln =>
              make(TypeSafeConfigSource.hoccon(Left(conf)), configDescriptor)
                .mapError(r => new RuntimeException(s"${ln}${r.mkString(ln)}"))
          )
      )

}
