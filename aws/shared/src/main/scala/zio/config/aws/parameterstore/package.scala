package zio.config.aws

import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import izumi.reflect.Tag
import zio.config._
import zio.{Has, Layer, Task}

package object parameterstore {
  implicit class FromConfigTypesafe(c: ZConfig.type) {
    def fromParameterStore[A](
      configDescriptor: ConfigDescriptor[A],
      basePath: String,
      getClient: Task[AWSSimpleSystemsManagement] = Task(AWSSimpleSystemsManagementClientBuilder.defaultClient())
    )(implicit tag: Tag[A]): Layer[ReadError[String], Has[A]] =
      ParameterStoreConfig.from(configDescriptor, basePath, getClient)
  }

  implicit class FromConfigSourceTypesafe(c: ConfigSource.type) {
    def fromParameterStore(
      basePath: String,
      getClient: Task[AWSSimpleSystemsManagement] = Task(AWSSimpleSystemsManagementClientBuilder.defaultClient())
    ): ConfigSource =
      ParameterStoreConfigSource.from(basePath, getClient)
  }
}
