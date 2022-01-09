package zio.config.aws.parameterstore

import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import zio.config.{ReadError, _}
import zio.{Has, Layer, Tag, Task}

object ParameterStoreConfig {
  def from[A](
    configDescriptor: ConfigDescriptor[A],
    basePath: String,
    getClient: Task[AWSSimpleSystemsManagement] = Task(AWSSimpleSystemsManagementClientBuilder.defaultClient())
  )(implicit tag: Tag[A]): Layer[ReadError[String], Has[A]] =
    ZConfig.fromConfigDescriptor(
      configDescriptor from ParameterStoreConfigSource.from(basePath, getClient)
    )
}
