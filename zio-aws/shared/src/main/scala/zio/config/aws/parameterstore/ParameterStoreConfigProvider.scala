package zio.config.aws.parameterstore

import zio.aws.ssm.Ssm
import zio.aws.ssm.model.primitives.PSParameterName
import zio.aws.ssm.model.{GetParametersByPathRequest, Parameter}
import zio.{Chunk, Config, ConfigProvider, ZIO}

/**
 * Builds a `ConfigProvider` from the parameters under one path in AWS Systems Manager Parameter Store, with the zio-aws
 * `Ssm` client. `import zio.config.aws.parameterstore._` adds the same constructor to `ConfigProvider`, as
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
   * Parameter Store are not seen. It collects the whole stream that `Ssm#getParametersByPath` returns, with `recursive`
   * and `withDecryption` set to `true`, and leaves out a parameter that has no name or no value. Fails with a
   * `Config.Error.Unsupported` whose message is the `toString` of the `AwsError` when the AWS call fails.
   *
   * @param basePath
   *   the path to read, without a trailing `/`
   */
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
