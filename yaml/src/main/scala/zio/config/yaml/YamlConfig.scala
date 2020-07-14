package zio.config.yaml

import java.io.File
import java.nio.file.Path

import zio.{ Tag, ZLayer }
import zio.blocking.Blocking
import zio.config.{ Config, ConfigDescriptor }

object YamlConfig {
  def fromString[A: Tag](yamlString: String, descriptor: ConfigDescriptor[A]): ZLayer[Any, Throwable, Config[A]] =
    Config.fromConfigDescriptorM(
      YamlConfigSource.fromString(yamlString).map(descriptor from _)
    )

  def fromPath[A: Tag](path: Path, descriptor: ConfigDescriptor[A]): ZLayer[Blocking, Throwable, Config[A]] =
    fromFile(path.toFile, descriptor)

  def fromFile[A: Tag](file: File, descriptor: ConfigDescriptor[A]): ZLayer[Blocking, Throwable, Config[A]] =
    Config.fromConfigDescriptorM(
      YamlConfigSource.fromFile(file).map(descriptor from _)
    )
}
