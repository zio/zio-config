package zio.config

import com.github.ghik.silencer.silent
import zio.{IO, Scope, ZIO}

import scala.collection.mutable.{Map => MutableMap}

import zio.ConfigProvider
import zio.Config

// Backward compatible approach to minimise the client changes
final case class Read[A](config: Config[A], configProvider: ConfigProvider)

@silent("Unused import")
private[config] trait ReadModule {
  import VersionSpecificSupport._

  final def read[A](reader: Read[A]): IO[Config.Error, A] =
    reader.configProvider.load(reader.config)

}
