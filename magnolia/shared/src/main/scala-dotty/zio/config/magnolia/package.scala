package zio.config

import zio.{Config, ConfigProvider}
import zio.IO

import scala.deriving.Mirror

package object magnolia {

  /**
   * Returns the `Config` description of a type, built from the type's `DeriveConfig` instance.
   *
   * For a case class, each field is read under a key with the field's name. A field with a default value uses it when
   * the key is missing, and an `Option` field gives `None`. For a sealed trait, a case object is read as its name, and
   * a case class under a key with its name. The annotations in `zio.config.derivation` (`@name`, `@describe`,
   * `@discriminator`, `@kebabCase`, `@kebabCaseLegacy`, `@snakeCase`, `@prefix` and `@postfix`) change these rules. On
   * Scala 2, a value class (`extends AnyVal`) is read as its single field; on Scala 3, it needs its own instance.
   * Compilation fails when the type of a field has no instance.
   *
   * {{{
   * import zio._
   * import zio.config.magnolia._
   *
   * final case class Database(url: String, poolSize: Int = 10)
   * final case class AppConfig(name: String, database: Database, replicas: Option[List[String]])
   *
   * val loaded: IO[Config.Error, AppConfig] =
   *   ConfigProvider
   *     .fromMap(Map("name" -> "orders", "database.url" -> "jdbc:postgresql://localhost/orders"))
   *     .load(deriveConfig[AppConfig])
   * // succeeds with AppConfig(orders,Database(jdbc:postgresql://localhost/orders,10),None)
   * }}}
   *
   * @see
   *   [[https://zio.dev/zio-config/automatic-derivation-of-config/ Automatic Derivation of Config]]
   */
  def deriveConfig[A](implicit ev: DeriveConfig[A]) =
    ev.desc

  /**
   * The `@describe` annotation, `zio.config.derivation.describe`, re-exported so that `import zio.config.magnolia._`
   * brings it into scope with `deriveConfig`.
   *
   * {{{
   * import zio.Config
   * import zio.config.magnolia._
   *
   * final case class Server(@describe("Port the server listens on") port: Int)
   *
   * val server: Config[Server] = deriveConfig[Server]
   * }}}
   */
  type describe = derivation.describe

  /** The companion of the `@describe` annotation, re-exported with the type. */
  val describe: derivation.describe.type = derivation.describe

  /**
   * The `@name` annotation, `zio.config.derivation.name`, re-exported so that `import zio.config.magnolia._` brings it
   * into scope with `deriveConfig`.
   *
   * {{{
   * import zio.Config
   * import zio.config.magnolia._
   *
   * final case class Server(@name("listen-port") port: Int)
   *
   * val server: Config[Server] = deriveConfig[Server]
   * }}}
   */
  type name = derivation.name

  /** The companion of the `@name` annotation, re-exported with the type. */
  val name: derivation.name.type = derivation.name

  /**
   * The `@kebabCaseLegacy` annotation, `zio.config.derivation.kebabCaseLegacy`, re-exported so that
   * `import zio.config.magnolia._` brings it into scope with `deriveConfig`.
   *
   * {{{
   * import zio.Config
   * import zio.config.magnolia._
   *
   * @kebabCaseLegacy
   * final case class Api(version2Name: String)
   *
   * val api: Config[Api] = deriveConfig[Api]
   * }}}
   */
  type kebabCaseLegacy = derivation.kebabCaseLegacy

  /** The companion of the `@kebabCaseLegacy` annotation, re-exported with the type. */
  val kebabCaseLegacy: derivation.kebabCaseLegacy.type = derivation.kebabCaseLegacy

  /**
   * The `@discriminator` annotation, `zio.config.derivation.discriminator`, re-exported so that
   * `import zio.config.magnolia._` brings it into scope with `deriveConfig`.
   *
   * {{{
   * import zio.Config
   * import zio.config.magnolia._
   *
   * @discriminator("kind")
   * sealed trait Storage
   * final case class S3(bucket: String) extends Storage
   * case object InMemory extends Storage
   *
   * final case class App(storage: Storage)
   *
   * val app: Config[App] = deriveConfig[App]
   * }}}
   */
  type discriminator = derivation.discriminator

  /** The companion of the `@discriminator` annotation, re-exported with the type. */
  val discriminator: derivation.discriminator.type = derivation.discriminator

  /**
   * Uses an implicit or given `Config[A]` as the `DeriveConfig[A]`, so that derivation reads a field of type `A` with
   * it. Implicit search finds it only where it is imported, for example with `import zio.config.magnolia._`.
   */
  implicit def deriveConfigFromConfig[A](implicit ev: Config[A]): DeriveConfig[A] =
    DeriveConfig(ev, None)

  /** Adds `autoLoad` to a `ConfigProvider`. */
  implicit class ConfigProviderOps[A](configProvider: ConfigProvider) {

    /**
     * Loads an `A` from this provider with its `DeriveConfig` instance; the same as
     * `configProvider.load(deriveConfig[A])`.
     */
    def autoLoad[A: DeriveConfig]: IO[Config.Error, A] =
      configProvider.load(DeriveConfig[A].desc)
  }

  /**
   * Enables derivation of `Config` via the `derives Config` syntax: `derived` builds a `Config[A]` with the same rules
   * as `deriveConfig`.
   *
   * {{{
   * // scala-3-only
   * import zio._
   * import zio.config.magnolia._
   *
   * final case class MyConfig(host: String, port: Int = 8080) derives Config
   *
   * val loaded: IO[Config.Error, MyConfig] = ConfigProvider.fromMap(Map("host" -> "localhost")).autoLoad[MyConfig]
   * // succeeds with MyConfig(localhost,8080)
   * }}}
   */
  extension (? : Config.type) {

    /** Derives a `Config[A]` with the same rules as `deriveConfig`; `derives Config` calls it. */
    inline def derived[A](using Mirror.Of[A]): Config[A] = DeriveConfig.derived[A].desc
  }

}
