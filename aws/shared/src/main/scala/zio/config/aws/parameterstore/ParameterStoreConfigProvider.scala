package zio.config.aws.parameterstore

import com.amazonaws.services.simplesystemsmanagement.model.{GetParametersByPathRequest, Parameter}
import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import zio.stream.ZStream
import zio.{Chunk, Config, ConfigProvider, Task, ZIO}

import scala.jdk.CollectionConverters._

/**
 * Builds a `ConfigProvider` from the parameters under one path in AWS Systems Manager Parameter Store, with the AWS SDK
 * for Java v1 client. `import zio.config.aws.parameterstore._` adds the same constructor to `ConfigProvider`, as
 * `ConfigProvider.fromParameterStore`.
 *
 * Each parameter name loses its `basePath/` prefix, and the remaining `/`-separated parts become the config path, so
 * `/myapp/prod/db/url` read under `/myapp/prod` is found by `Config.string("url").nested("db")`. The prefix is removed
 * with `String#replaceFirst`, so `basePath` is read as a regular expression: a `.` in it matches any character. Pass
 * `basePath` without a trailing `/`: with `/myapp/prod/`, no prefix is removed and the keys are not found.
 *
 * @see
 *   [[https://zio.dev/zio-config/aws-parameter-store/ AWS Parameter Store]]
 */
object ParameterStoreConfigProvider {

  /**
   * Reads every parameter under `basePath` once and returns a `ConfigProvider` over their values, so later changes in
   * Parameter Store are not seen. It calls `GetParametersByPath` with `recursive` and `withDecryption` set to `true`,
   * and follows `NextToken` until the last page. Fails with a `Config.Error.Unsupported` whose message is the
   * `toString` of the exception when `getClient` fails or an AWS call throws.
   *
   * @param basePath
   *   the path to read, without a trailing `/`
   * @param getClient
   *   builds the client. The default, `AWSSimpleSystemsManagementClientBuilder.defaultClient()`, takes the credentials
   *   and the region from the AWS SDK's default provider chains.
   */
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
