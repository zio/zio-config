package zio.config.typesafe

import zio.config._, Config._
import java.io.File
import zio.ZIO
import com.typesafe.config.ConfigFactory
import zio.system.System.Live.system.lineSeparator
import zio.Task

object TypesafeConfig {
  def fromHocconFile[A](configDescriptor: ConfigDescriptor[String, String, A], file: File) =
    ZIO
      .effect(ConfigFactory.parseFile(file).resolve)
      .flatMap(
        conf =>
          lineSeparator.flatMap(
            ln =>
              make(TypeSafeConfigSource.hoccon(Left(conf)), configDescriptor)
                .mapError(r => new RuntimeException(s"${ln}${r.mkString(ln)}"))
          )
      )

  def fromHocconString[A](str: String, configDescriptor: ConfigDescriptor[String, String, A]): Task[Config[A]] =
    ZIO
      .effect(ConfigFactory.parseString(str).resolve)
      .flatMap(
        conf =>
          lineSeparator.flatMap(
            ln =>
              make(TypeSafeConfigSource.hoccon(Left(conf)), configDescriptor)
                .mapError(r => new RuntimeException(s"${ln}${r.mkString(ln)}"))
          )
      )

}
