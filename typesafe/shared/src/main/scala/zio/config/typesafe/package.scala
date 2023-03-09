package zio.config

import zio._

import java.io.File

package object typesafe {

  implicit class FromConfigSourceTypesafe(configProvider: ConfigProvider.type) {
    def fromResourcePath(enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TypesafeConfigProvider.fromResourcePath(enableCommaSeparatedValueAsList)

    def fromHoconFile[A](file: File, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TypesafeConfigProvider.fromHoconFile(file, enableCommaSeparatedValueAsList)

    def fromHoconFilePath[A](filePath: String, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TypesafeConfigProvider.fromHoconFilePath(filePath, enableCommaSeparatedValueAsList)

    def fromHoconString(input: String, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TypesafeConfigProvider.fromHoconString(input, enableCommaSeparatedValueAsList)

    def fromTypesafeConfig(
      rawConfig: com.typesafe.config.Config,
      enableCommaSeparatedValueAsList: Boolean = false
    ): ConfigProvider =
      TypesafeConfigProvider.fromTypesafeConfig(rawConfig, enableCommaSeparatedValueAsList)
  }

}
