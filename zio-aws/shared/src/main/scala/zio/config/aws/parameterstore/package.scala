package zio.config.aws

import io.github.vigoo.zioaws.ssm.Ssm
import izumi.reflect.Tag
import zio.config._
import zio.{Has, ZLayer}

package object parameterstore {
  implicit class FromConfigTypesafe(c: ZConfig.type) {
    def fromParameterStore[A](
      configDescriptor: ConfigDescriptor[A],
      basePath: String
    )(implicit tag: Tag[A]): ZLayer[Ssm, ReadError[String], Has[A]] =
      ParameterStoreConfig.from(configDescriptor, basePath)
  }

  implicit class FromConfigSourceTypesafe(c: ConfigSource.type) {
    def fromParameterStore(
      basePath: String,
      ssm: Ssm.Service
    ): ConfigSource =
      ParameterStoreConfigSource.from(basePath, ssm)
  }
}
