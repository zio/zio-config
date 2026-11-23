package zio.config

import zio.Config

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
  def deriveConfig[T](implicit config: DeriveConfig[T]): Config[T] =
    config.desc

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
   * The `@kebabCase` annotation, `zio.config.derivation.kebabCase`, re-exported so that `import zio.config.magnolia._`
   * brings it into scope with `deriveConfig`. Scala 3 doesn't re-export it: import it from `zio.config.derivation`
   * there.
   *
   * {{{
   * import zio.Config
   * import zio.config.magnolia._
   *
   * @kebabCase
   * final case class Person(firstName: String)
   *
   * val person: Config[Person] = deriveConfig[Person]
   * }}}
   */
  type kebabCase = derivation.kebabCase

  /** The companion of the `@kebabCase` annotation, re-exported with the type. */
  val kebabCase: derivation.kebabCase.type = derivation.kebabCase

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
   * The `@snakeCase` annotation, `zio.config.derivation.snakeCase`, re-exported so that `import zio.config.magnolia._`
   * brings it into scope with `deriveConfig`. Scala 3 doesn't re-export it: import it from `zio.config.derivation`
   * there.
   *
   * {{{
   * import zio.Config
   * import zio.config.magnolia._
   *
   * @snakeCase
   * final case class Person(firstName: String)
   *
   * val person: Config[Person] = deriveConfig[Person]
   * }}}
   */
  type snakeCase = derivation.snakeCase

  /** The companion of the `@snakeCase` annotation, re-exported with the type. */
  val snakeCase: derivation.snakeCase.type = derivation.snakeCase

  /**
   * The `@prefix` annotation, `zio.config.derivation.prefix`, re-exported so that `import zio.config.magnolia._` brings
   * it into scope with `deriveConfig`. Scala 3 doesn't re-export it: import it from `zio.config.derivation` there.
   *
   * {{{
   * import zio.Config
   * import zio.config.magnolia._
   *
   * @prefix("db")
   * final case class Pool(poolSize: Int)
   *
   * val pool: Config[Pool] = deriveConfig[Pool]
   * }}}
   */
  type prefix = derivation.prefix

  /** The companion of the `@prefix` annotation, re-exported with the type. */
  val prefix: derivation.prefix.type = derivation.prefix

  /**
   * The `@postfix` annotation, `zio.config.derivation.postfix`, re-exported so that `import zio.config.magnolia._`
   * brings it into scope with `deriveConfig`. Scala 3 doesn't re-export it: import it from `zio.config.derivation`
   * there.
   *
   * {{{
   * import zio.Config
   * import zio.config.magnolia._
   *
   * @postfix("ms")
   * final case class Timeouts(connect: Long)
   *
   * val timeouts: Config[Timeouts] = deriveConfig[Timeouts]
   * }}}
   */
  type postfix = derivation.postfix

  /** The companion of the `@postfix` annotation, re-exported with the type. */
  val postfix: derivation.postfix.type = derivation.postfix
}
