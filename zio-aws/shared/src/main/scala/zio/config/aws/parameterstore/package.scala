package zio.config.aws

import zio.aws.ssm.Ssm
import zio.{Config, ConfigProvider, ZIO}

package object parameterstore {

  /**
   * Adds `fromParameterStore` to the `ConfigProvider` companion, so that `import zio.config.aws.parameterstore._` makes
   * `ConfigProvider.fromParameterStore` available.
   */
  implicit class FromConfigSourceTypesafe(c: ConfigProvider.type) {

    /**
     * Reads every parameter under `basePath` once and returns a `ConfigProvider` over their values, so later changes in
     * Parameter Store are not seen. It collects the whole stream that `Ssm#getParametersByPath` returns, with
     * `recursive` and `withDecryption` set to `true`, and leaves out a parameter that has no name or no value. Fails
     * with a `Config.Error.Unsupported` whose message is the `toString` of the `AwsError` when the AWS call fails.
     *
     * Each parameter name loses its `basePath/` prefix, and the remaining `/`-separated parts become the config path,
     * so `/myapp/prod/db/url` read under `/myapp/prod` is found by `Config.string("url").nested("db")`. The prefix is
     * removed with `String#replaceFirst`, so `basePath` is read as a regular expression: a `.` in it matches any
     * character. Pass `basePath` without a trailing `/`: with `/myapp/prod/`, no prefix is removed and the keys are not
     * found.
     *
     * @param basePath
     *   the path to read, without a trailing `/`
     */
    def fromParameterStore(
      basePath: String,
      ssm: Ssm
    ): ZIO[Any, Config.Error, ConfigProvider] =
      ParameterStoreConfigProvider.from(basePath, ssm)
  }
}
