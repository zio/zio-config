package zio.config.aws.parameterstore

import io.github.vigoo.zioaws.ssm.Ssm
import io.github.vigoo.zioaws.ssm.model.{GetParametersByPathRequest, Parameter}
import zio.config.{PropertyTreePath, ReadError, _}
import zio.{Chunk, ZIO, ZManaged}

import ConfigSource._

object ParameterStoreConfigSource {
  def from(
    basePath: String,
    ssm: Ssm.Service
  ): ConfigSource = {
    val effect: MemoizableManagedReader =
      ZManaged.succeed {
        ZManaged
          .fromEffect(
            ssm
              .getParametersByPath(
                GetParametersByPathRequest(path = basePath, recursive = Some(true), withDecryption = Some(true))
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
      parameter.nameValue.flatMap { name =>
        parameter.valueValue.map { value =>
          name.replaceFirst(str, "") -> value
        }
      }
    }.toMap
  }
}
