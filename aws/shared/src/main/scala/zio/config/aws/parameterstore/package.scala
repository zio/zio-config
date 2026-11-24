package zio.config.aws

import com.amazonaws.services.simplesystemsmanagement.{
  AWSSimpleSystemsManagement,
  AWSSimpleSystemsManagementClientBuilder
}
import zio._

package object parameterstore {

  /**
   * Adds `fromParameterStore` to the `ConfigProvider` companion, so that `import zio.config.aws.parameterstore._` makes
   * `ConfigProvider.fromParameterStore` available.
   */
  implicit class FromConfigSourceTypesafe(c: ConfigProvider.type) {

    /**
     * Reads every parameter under `basePath` once and returns a `ConfigProvider` over their values, so later changes in
     * Parameter Store are not seen. It calls `GetParametersByPath` with `recursive` and `withDecryption` set to `true`,
     * and follows `NextToken` until the last page. Fails with a `Config.Error.Unsupported` whose message is the
     * `toString` of the exception when `getClient` fails or an AWS call throws.
     *
     * Each parameter name loses its `basePath/` prefix, and the remaining `/`-separated parts become the config path,
     * so `/myapp/prod/db/url` read under `/myapp/prod` is found by `Config.string("url").nested("db")`. The prefix is
     * removed with `String#replaceFirst`, so `basePath` is read as a regular expression: a `.` in it matches any
     * character. Pass `basePath` without a trailing `/`: with `/myapp/prod/`, no prefix is removed and the keys are not
     * found.
     *
     * @param basePath
     *   the path to read, without a trailing `/`
     * @param getClient
     *   builds the client. The default, `AWSSimpleSystemsManagementClientBuilder.defaultClient()`, takes the
     *   credentials and the region from the AWS SDK's default provider chains.
     */
    def fromParameterStore(
      basePath: String,
      getClient: Task[AWSSimpleSystemsManagement] = ZIO.attempt(AWSSimpleSystemsManagementClientBuilder.defaultClient())
    ): ZIO[Any, Config.Error, ConfigProvider] =
      ParameterStoreConfigProvider.from(basePath, getClient)
  }
}
