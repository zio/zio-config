package zio.config.aws.parameterstore

import zio.aws.ssm.Ssm
import zio.aws.ssm.model.primitives.PSParameterName
import zio.aws.ssm.model.{GetParametersByPathRequest, Parameter}
import zio.config.{PropertyTreePath, ReadError, _}
import zio.{Chunk, ZIO}

import ConfigSource._

object ParameterStoreConfigSource {
  def from(
    basePath: String,
    ssm: Ssm
  ): ConfigSource = {
    val effect: MemoizableManagedReader =
      ZIO.succeed {
        ssm
          .getParametersByPath(
            GetParametersByPathRequest(path = PSParameterName(basePath), recursive = true, withDecryption = true)
          )
          .runCollect
          .map { result =>
            ConfigSource
              .getPropertyTreeFromMap(
                convertParameterListToMap(result, basePath),
                keyDelimiter = Some('/')
              )
          }
          .mapBoth(
            throwable => ReadError.SourceError(throwable.toString): ReadError[String],
            tree => (path: PropertyTreePath[String]) => ZIO.succeed(tree.at(path))
          )
      }

    ConfigSource
      .Reader(
        Set(ConfigSourceName("parameter-store")),
        effect
      )
  }

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
