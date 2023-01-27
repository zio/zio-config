package zio.config.aws

import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import zio._
import zio.config._

package object parameterstore {
  implicit class FromConfigTypesafe(c: ZConfig.type) {
    def fromParameterStore[A](
      configDescriptor: Config[A],
      basePath: String,
      getClient: Task[AWSSimpleSystemsManagement] = ZIO.attempt(AWSSimpleSystemsManagementClientBuilder.defaultClient())
    )(implicit tag: Tag[A]): Layer[Config.Error, A] =
      ParameterStoreConfig.from(configDescriptor, basePath, getClient)
  }

  implicit class FromConfigSourceTypesafe(c: ConfigSource.type) {
    def fromParameterStore(
      basePath: String,
      getClient: Task[AWSSimpleSystemsManagement] = ZIO.attempt(AWSSimpleSystemsManagementClientBuilder.defaultClient())
    ): ConfigSource =
      ParameterStoreConfigSource.from(basePath, getClient)
  }
}
