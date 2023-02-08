package zio.config.syntax

import zio.ConfigProvider
import zio.Chunk
import zio.{Config}
import zio.{IO, Trace}

// ConfigProvider that works with IndexedFlat
trait ConfigProvider0 extends ConfigProvider { self =>

  /**
   * Flattens this config provider into a simplified config provider that knows
   * only how to deal with flat (key/value) properties.
   */
  def indexedFlat: IndexedFlat

  /**
   * Returns a new config provider that will automatically nest all
   * configuration under the specified property name. This can be utilized to
   * aggregate separate configuration sources that are all required to load a
   * single configuration value.
   */
  final def nested_(name: String): ConfigProvider =
    ConfigProvider.fromIndexedFlat(self.indexedFlat.nested(KeyComponent.KeyName(name)))

  final def at(path: Chunk[KeyComponent]): ConfigProvider0 =
    ConfigProvider.fromIndexedFlat(self.indexedFlat.nested_(path))

  /**
   * Returns a new config provider that preferentially loads configuration data
   * from this one, but which will fall back to the specified alterate provider
   * if there are any issues loading the configuration from this provider.
   */
  final def orElse_(that: ConfigProvider0): ConfigProvider =
    ConfigProvider.fromIndexedFlat(self.indexedFlat.orElse(that.indexedFlat))

}
