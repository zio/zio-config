package zio.config.aws

import zio.aws.ssm.Ssm
import zio.config._
import zio.{Config, ConfigProvider, ZIO}

package object parameterstore {

  implicit class FromConfigSourceTypesafe(c: ConfigProvider.type) {
    def fromParameterStore(
      basePath: String,
      ssm: Ssm
    ): ZIO[Any, Config.Error, ConfigProvider] =
      ParameterStoreConfigProvider.from(basePath, ssm)
  }
}
