package zio.config.aws.parameterstore

import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import zio.config.{ReadError, _}
import zio._

object ParameterStoreConfig {
  def from[A](
    configDescriptor: ConfigDescriptor[A],
    basePath: String,
    getClient: Task[AWSSimpleSystemsManagement] = Task(AWSSimpleSystemsManagementClientBuilder.defaultClient())
  )(implicit tag: Tag[A], ev: IsNotIntersection[A]): Layer[ReadError[String], A] =
    ZConfig.fromConfigDescriptor(
      configDescriptor from ParameterStoreConfigSource.from(basePath, getClient)
    )
}
