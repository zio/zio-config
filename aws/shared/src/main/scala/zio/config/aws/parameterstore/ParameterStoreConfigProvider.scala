package zio.config.aws.parameterstore

import com.amazonaws.services.simplesystemsmanagement.model.{GetParametersByPathRequest, Parameter}
import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import zio.stream.ZStream
import zio.{Chunk, Task, ZIO}

import scala.jdk.CollectionConverters._

import zio.ConfigProvider
import zio.Config

object ParameterStoreConfigProvider {
  def from(
    basePath: String,
    getClient: Task[AWSSimpleSystemsManagement] = ZIO.attempt(AWSSimpleSystemsManagementClientBuilder.defaultClient())
  ): ZIO[Any, Config.Error, ConfigProvider] =
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
          ConfigProvider
            .fromMap(
              convertParameterListToMap(result.flatten.toList, basePath),
              pathDelim = "/"
            )
        }
    }
      .mapError(throwable => Config.Error.Unsupported(message = throwable.toString): Config.Error)

  private[config] def convertParameterListToMap(list: List[Parameter], basePath: String): Map[String, String] = {
    val str = s"$basePath/"
    list.map(parameter => (parameter.getName.replaceFirst(str, ""), parameter.getValue)).toMap
  }
}
