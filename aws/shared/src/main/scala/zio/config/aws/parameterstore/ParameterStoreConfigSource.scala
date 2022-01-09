package zio.config.aws.parameterstore

import com.amazonaws.services.simplesystemsmanagement.model.{GetParametersByPathRequest, Parameter}
import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import zio.config.PropertyTreePath.Step
import zio.config.{PropertyTreePath, ReadError, _}
import zio.{Task, ZIO, ZManaged}

import scala.jdk.CollectionConverters._

import ConfigSource._

object ParameterStoreConfigSource {
  def from(
    basePath: String,
    getClient: Task[AWSSimpleSystemsManagement] = Task(AWSSimpleSystemsManagementClientBuilder.defaultClient())
  ): ConfigSource = {
    val effect: MemoizableManagedReader =
      ZManaged.succeed {
        ZManaged
          .fromEffect(
            getClient
          )
          .map(ssm =>
            (path: PropertyTreePath[String]) =>
              (for {
                request <-
                  ZIO
                    .effect(
                      new GetParametersByPathRequest()
                        .withPath(s"${basePath}/${convertPathToString(path)}")
                        .withRecursive(true)
                        .withMaxResults(1000)
                        .withWithDecryption(true)
                    )

                tree <-
                  ZIO
                    .effect(ssm.getParametersByPath(request).getParameters)
                    .map(_.asScala.toList)
                    .map { list =>
                      ConfigSource.getPropertyTreeFromMap(toMap(list, basePath), keyDelimiter = Some('/'))
                    }
              } yield tree)
                .mapError(throwable => ReadError.SourceError(throwable.toString): ReadError[String])
          )
          .mapError(throwable => ReadError.SourceError(throwable.toString): ReadError[String])
      }

    ConfigSource
      .Reader(
        Set(ConfigSourceName("parameter-store")),
        effect
      )
  }

  def toMap(list: List[Parameter], basePath: String): Map[String, String] =
    list.map(parameter => (parameter.getName.replaceFirst(basePath, ""), parameter.getValue)).toMap

  def convertPathToString(propertyTreePath: PropertyTreePath[String]): String =
    propertyTreePath.path
      .map({
        case Step.Index(_) => ""
        case Step.Key(k)   => k
      })
      .mkString("/")
}
