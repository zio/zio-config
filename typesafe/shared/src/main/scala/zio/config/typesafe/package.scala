package zio.config

import zio._

import java.io.File
import zio.config.syntax.IndexedConfigProvider

package object typesafe {

  implicit class FromConfigSourceTypesafe(configProvider: ConfigProvider.type) {
    def fromResourcePath: IndexedConfigProvider =
      TypesafeConfigSource.fromResourcePath

    def fromHoconFile[A](file: File): IndexedConfigProvider =
      TypesafeConfigSource.fromHoconFile(file)

    def fromHoconFilePath[A](filePath: String): IndexedConfigProvider =
      TypesafeConfigSource.fromHoconFilePath(filePath)

    def fromHoconString(input: String): IndexedConfigProvider =
      TypesafeConfigSource.fromHoconString(input)

    def fromTypesafeConfig(
      rawConfig: com.typesafe.config.Config
    ): IndexedConfigProvider =
      TypesafeConfigSource.fromTypesafeConfig(rawConfig)
  }

}
