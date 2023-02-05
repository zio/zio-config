package zio.config.aws

import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import zio._
import zio.config._

package object parameterstore {

  implicit class FromConfigSourceTypesafe(c: ConfigProvider.type) {
    def fromParameterStore(
      basePath: String,
      getClient: Task[AWSSimpleSystemsManagement] = ZIO.attempt(AWSSimpleSystemsManagementClientBuilder.defaultClient())
    ): ZIO[Any, Config.Error, ConfigProvider] =
      ParameterStoreConfigProvider.from(basePath, getClient)
  }
}
