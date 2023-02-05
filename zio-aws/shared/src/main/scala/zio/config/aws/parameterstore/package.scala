package zio.config.aws

import zio.aws.ssm.Ssm
import zio.config._
import zio.ConfigProvider
import zio.{ZIO, Config}

package object parameterstore {

  implicit class FromConfigSourceTypesafe(c: ConfigProvider.type) {
    def fromParameterStore(
      basePath: String,
      ssm: Ssm
    ): ZIO[Any, Config.Error, ConfigProvider] =
      ParameterStoreConfigSource.from(basePath, ssm)
  }
}
