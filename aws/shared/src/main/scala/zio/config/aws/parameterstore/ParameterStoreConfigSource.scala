package zio.config.aws.parameterstore

import com.amazonaws.services.simplesystemsmanagement.model.{GetParametersByPathRequest, Parameter}
import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import zio.config.{PropertyTreePath, ReadError, _}
import zio.stream.ZStream
import zio.{Chunk, Task, ZIO}

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

          ZStream
            .paginateZIO(
              ZIO.attempt(ssm.getParametersByPath(request))
            )(_.map { response =>
              val currentBatchResult =
                Chunk.fromIterable(
                  response.getParameters.asScala.toList
                )
              val nextToken          = response.getNextToken
              val nextBatch          =
                if (nextToken == null || nextToken.trim.isEmpty)
                  None
                else
                  Some(
                    ZIO.attempt(
                      ssm.getParametersByPath(request.withNextToken(nextToken))
                    )
                  )

              (currentBatchResult, nextBatch)
            })
            .runCollect
            .map { result =>
              ConfigSource
                .getPropertyTreeFromMap(
                  convertParameterListToMap(result.flatten.toList, basePath),
                  keyDelimiter = Some('/')
                )
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
