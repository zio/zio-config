package zio.config.aws.parameterstore

import zio.aws.ssm.Ssm
import zio.config.{ReadError, _}
import zio.{Tag, ZIO, ZLayer}

object ParameterStoreConfig {
  def from[A](
    configDescriptor: ConfigDescriptor[A],
    basePath: String
  )(implicit tag: Tag[A]): ZLayer[Ssm, ReadError[String], A] =
    ZConfig.fromConfigDescriptorM(
      ZIO.service[Ssm].map { ssm =>
        configDescriptor from ParameterStoreConfigSource.from(basePath, ssm)
      }
    )
}
