package zio.config

import zio._

import java.io.File

package object typesafe {

  implicit class FromConfigSourceTypesafe(configProvider: ConfigProvider.type) {
    def fromResourcePath: ConfigProvider =
      TypesafeConfigProvider.fromResourcePath

    def fromHoconFile[A](file: File): ConfigProvider =
      TypesafeConfigProvider.fromHoconFile(file)

    def fromHoconFilePath[A](filePath: String): ConfigProvider =
      TypesafeConfigProvider.fromHoconFilePath(filePath)

    def fromHoconString(input: String): ConfigProvider =
      TypesafeConfigProvider.fromHoconString(input)

    def fromTypesafeConfig(
      rawConfig: com.typesafe.config.Config
    ): ConfigProvider =
      TypesafeConfigProvider.fromTypesafeConfig(rawConfig)
  }

}
