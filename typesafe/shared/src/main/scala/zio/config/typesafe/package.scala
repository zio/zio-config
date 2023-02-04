package zio.config

import zio._

import java.io.File

package object typesafe {

  implicit class FromConfigSourceTypesafe(configProvider: ConfigProvider.type) {
    def fromResourcePath: ConfigProvider =
      TypesafeConfigSource.fromResourcePath

    def fromHoconFile[A](file: File): ConfigProvider =
      TypesafeConfigSource.fromHoconFile(file)

    def fromHoconFilePath[A](filePath: String): ConfigProvider =
      TypesafeConfigSource.fromHoconFilePath(filePath)

    def fromHoconString(input: String): syntax.ConfigProvider0 =
      TypesafeConfigSource.fromHoconString(input)

    def fromTypesafeConfig(
      rawConfig: com.typesafe.config.Config
    ): ConfigProvider =
      TypesafeConfigSource.fromTypesafeConfig(rawConfig)
  }

}
