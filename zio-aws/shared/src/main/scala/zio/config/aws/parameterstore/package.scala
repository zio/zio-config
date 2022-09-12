package zio.config.aws

import zio.aws.ssm.Ssm
import zio.config._
import zio.{Tag, ZLayer}

package object parameterstore {
  implicit class FromConfigTypesafe(c: ZConfig.type) {
    def fromParameterStore[A](
      configDescriptor: ConfigDescriptor[A],
      basePath: String
    )(implicit tag: Tag[A]): ZLayer[Ssm, ReadError[String], A] =
      ParameterStoreConfig.from(configDescriptor, basePath)
  }

  implicit class FromConfigSourceTypesafe(c: ConfigSource.type) {
    def fromParameterStore(
      basePath: String,
      ssm: Ssm
    ): ConfigSource =
      ParameterStoreConfigSource.from(basePath, ssm)
  }
}
