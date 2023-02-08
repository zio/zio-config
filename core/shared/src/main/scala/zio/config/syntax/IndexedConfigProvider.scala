package zio.config.syntax

import zio.ConfigProvider
import zio.Chunk
import zio.{Config}
import zio.{IO, Trace}

// ConfigProvider that works with IndexedFlat
trait IndexedConfigProvider extends ConfigProvider { self =>

  /**
   * More complex sources will piggy back on an eriched Flat
   * that knows about the index of each config keys
   */
  def indexedFlat: IndexedFlat

  final def at(path: Chunk[KeyComponent]): IndexedConfigProvider =
    ConfigProvider.fromIndexedFlat(self.indexedFlat.nested(path))

  /**
   * Returns a new config provider that preferentially loads configuration data
   * from this one, but which will fall back to the specified alterate provider
   * if there are any issues loading the configuration from this provider.
   */
  final def orElse(that: IndexedConfigProvider): ConfigProvider =
    ConfigProvider.fromIndexedFlat(self.indexedFlat.orElse(that.indexedFlat))

}
