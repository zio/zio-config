package zio.config.aws.parameterstore

import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import zio._
import zio.config.{ReadError, _}

object ParameterStoreConfig {
  def from[A](
    configDescriptor: Config[A],
    basePath: String,
    getClient: Task[AWSSimpleSystemsManagement] = ZIO.attempt(AWSSimpleSystemsManagementClientBuilder.defaultClient())
  )(implicit tag: Tag[A]): Layer[Config.Error, A] =
    ZConfig.fromConfig(
      configDescriptor from ParameterStoreConfigSource.from(basePath, getClient)
    )
}
