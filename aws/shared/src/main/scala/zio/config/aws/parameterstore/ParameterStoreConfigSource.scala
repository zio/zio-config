package zio.config.aws.parameterstore

import com.amazonaws.services.simplesystemsmanagement.model.{GetParametersByPathRequest, Parameter}
import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import zio.config.{PropertyTreePath, ReadError, _}
import zio.{Task, ZIO}

import scala.jdk.CollectionConverters._

import ConfigSource._

object ParameterStoreConfigSource {
  def from(
    basePath: String,
    getClient: Task[AWSSimpleSystemsManagement] = ZIO.attempt(AWSSimpleSystemsManagementClientBuilder.defaultClient())
  ): ConfigSource = {
    val effect: MemoizableManagedReader =
      ZIO.succeed {
        getClient.flatMap { ssm =>
          val request =
            new GetParametersByPathRequest()
              .withPath(basePath)
              .withRecursive(true)
              .withWithDecryption(true)

          ZIO
            .attempt(ssm.getParametersByPath(request).getParameters)
            .map(_.asScala.toList)
            .map { list =>
              ConfigSource
                .getPropertyTreeFromMap(convertParameterListToMap(list, basePath), keyDelimiter = Some('/'))
            }
        }
          .map(tree => (path: PropertyTreePath[String]) => ZIO.succeed(tree.at(path)))
          .mapError(throwable => ReadError.SourceError(throwable.toString): ReadError[String])
      }

    ConfigSource
      .Reader(
        Set(ConfigSourceName("parameter-store")),
        effect
      )
  }

  private[config] def convertParameterListToMap(list: List[Parameter], basePath: String): Map[String, String] = {
    val str = s"$basePath/"
    list.map(parameter => (parameter.getName.replaceFirst(str, ""), parameter.getValue)).toMap
  }
}
