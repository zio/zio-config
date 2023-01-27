package zio.config.aws.parameterstore

import zio.aws.ssm.Ssm
import zio.config.{ReadError, _}
import zio.{Tag, ZIO, ZLayer}

object ParameterStoreConfig {
  def from[A](
    configDescriptor: Config[A],
    basePath: String
  )(implicit tag: Tag[A]): ZLayer[Ssm, Config.Error, A] =
    ZConfig.fromConfigM(
      ZIO.service[Ssm].map { ssm =>
        configDescriptor from ParameterStoreConfigSource.from(basePath, ssm)
      }
    )
}
