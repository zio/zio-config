package zio.config.aws.parameterstore

import zio.aws.ssm.Ssm
import zio.aws.ssm.model.primitives.PSParameterName
import zio.aws.ssm.model.{GetParametersByPathRequest, Parameter}
import zio.{Chunk, Config, ConfigProvider, ZIO}

object ParameterStoreConfigSource {
  def from(
    basePath: String,
    ssm: Ssm
  ): ZIO[Any, Config.Error, ConfigProvider] =
    ssm
      .getParametersByPath(
        GetParametersByPathRequest(path = PSParameterName(basePath), recursive = true, withDecryption = true)
      )
      .runCollect
      .map { result =>
        ConfigProvider
          .fromMap(
            convertParameterListToMap(result, basePath),
            pathDelim = "/"
          )
      }
      .mapError(throwable => Config.Error.Unsupported(message = throwable.toString): Config.Error)

  private[config] def convertParameterListToMap(
    list: Chunk[Parameter.ReadOnly],
    basePath: String
  ): Map[String, String] = {
    val str = s"$basePath/"
    list.flatMap { parameter =>
      parameter.name.flatMap { name =>
        parameter.value.map { value =>
          name.replaceFirst(str, "") -> value
        }
      }.toOption
    }.toMap
  }
}
