package zio.config.aws.parameterstore

import io.github.vigoo.zioaws.ssm.Ssm
import zio.config.{ReadError, _}
import zio.{Has, Tag, ZIO, ZLayer}

object ParameterStoreConfig {
  def from[A](
    configDescriptor: ConfigDescriptor[A],
    basePath: String
  )(implicit tag: Tag[A]): ZLayer[Ssm, ReadError[String], Has[A]] =
    ZConfig.fromConfigDescriptorM(
      ZIO.service[Ssm.Service].map { ssm =>
        configDescriptor from ParameterStoreConfigSource.from(basePath, ssm)
      }
    )
}
