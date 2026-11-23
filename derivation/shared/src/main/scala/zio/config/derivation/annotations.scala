package zio.config.derivation

import scala.annotation.StaticAnnotation

/**
 * Adds a description to what derivation produces. Descriptions appear in the output of `generateDocs`; they don't
 * change which keys are read.
 *
 * On a field, the text describes that field. On a case class, Scala 2 adds the text to the description of each of its
 * fields, and Scala 3 ignores it. On a sealed trait, it is ignored on Scala 2 and Scala 3.
 *
 * {{{
 * import zio.config._
 * import zio.config.magnolia.deriveConfig
 *
 * final case class HttpServer(@describe("Port the server listens on") port: Int, host: String)
 *
 * val docs: ConfigDocs = generateDocs(deriveConfig[HttpServer])
 * // the row for `port` has the description "an integer property, Port the server listens on"
 * }}}
 *
 * @param describe
 *   the text to add
 */
final case class describe(describe: String) extends StaticAnnotation

/**
 * Replaces the name that derivation uses for a field, a subtype of a sealed trait, or a sealed trait.
 *
 *   - On a field, the key is `name` instead of the field name. Key-case annotations, `@prefix` and `@postfix` on the
 *     class don't change it.
 *   - On a subtype, `name` replaces the subtype's name: it is the key that a case class is read under, and the value
 *     that selects a case object, or any subtype with `@discriminator`.
 *   - On a sealed trait, its subtypes are read under one more key, `name`.
 *
 * Scala 3 differs for subtypes. It also accepts the subtype's own name for a case object without `@discriminator` and
 * for a case class with `@discriminator`, and with `@discriminator` it ignores `@name` on a case object.
 *
 * {{{
 * import zio._
 * import zio.config.magnolia.deriveConfig
 *
 * final case class Server(@name("listen-port") port: Int, host: String)
 *
 * val server: IO[Config.Error, Server] =
 *   ConfigProvider.fromMap(Map("listen-port" -> "8080", "host" -> "localhost")).load(deriveConfig[Server])
 * // succeeds with Server(8080,localhost)
 * }}}
 *
 * @param name
 *   the key, or the name of the subtype
 */
final case class name(name: String) extends StaticAnnotation

/**
 * Reads the subtype of a sealed trait from the key `keyName`, with the fields of a case class next to it.
 *
 * Without this annotation, a case object is read as its name (`mode = Fast`), and a case class with fields is read
 * under a key with its name (`mode.Custom.level = 3`). With `@discriminator("type")` on the sealed trait, the value at
 * `type` names the subtype, and the names are compared case-sensitively. A name that matches no subtype, such as `Fax`,
 * fails with `Invalid case: Fax`.
 *
 * {{{
 * import zio._
 * import zio.config.magnolia.deriveConfig
 *
 * @discriminator("type")
 * sealed trait Notifier
 * final case class Email(address: String) extends Notifier
 * final case class Sms(number: String)    extends Notifier
 * case object Disabled                    extends Notifier
 *
 * final case class Alerts(notifier: Notifier)
 *
 * val sms: IO[Config.Error, Alerts] =
 *   ConfigProvider.fromMap(Map("notifier.type" -> "Sms", "notifier.number" -> "5550100")).load(deriveConfig[Alerts])
 * // succeeds with Alerts(Sms(5550100))
 *
 * val disabled: IO[Config.Error, Alerts] =
 *   ConfigProvider.fromMap(Map("notifier.type" -> "Disabled")).load(deriveConfig[Alerts])
 * // succeeds with Alerts(Disabled)
 * }}}
 *
 * The same `Sms` value in HOCON, with the `zio-config-typesafe` provider:
 *
 * ```hocon
 * notifier {
 *   type = Sms
 *   number = "5550100"
 * }
 * ```
 *
 * A case object needs only the key: `notifier { type = Disabled }`.
 *
 * @param keyName
 *   the key that holds the name of the subtype; `type` by default
 */
final case class discriminator(keyName: String = "type") extends StaticAnnotation

/**
 * Converts the field names of the annotated case class to kebab-case keys: `firstName` becomes `first-name`,
 * `version2Name` becomes `version2-name`, and `URLPath` becomes `urlpath`.
 *
 * It applies to the class's own fields, not to the fields of a nested case class (annotate that class too), and not to
 * a field with `@name`. Use one key-case annotation per class: with several, Scala 2 uses the first one, and Scala 3
 * prefers `@kebabCase`, then `@kebabCaseLegacy`, then `@snakeCase`.
 *
 * {{{
 * import zio._
 * import zio.config.magnolia.deriveConfig
 *
 * @kebabCase
 * final case class Person(firstName: String, lastName: String)
 *
 * val person: IO[Config.Error, Person] =
 *   ConfigProvider.fromMap(Map("first-name" -> "Ada", "last-name" -> "Lovelace")).load(deriveConfig[Person])
 * // succeeds with Person(Ada,Lovelace)
 * }}}
 */
final case class kebabCase() extends StaticAnnotation

/**
 * Converts the field names of the annotated case class to kebab-case keys with an older rule, which does not start a
 * new word at an uppercase letter that follows a digit: `version2Name` becomes `version2name`, where `@kebabCase` gives
 * `version2-name`. For `firstName`, `myValue123` and `instantIdQAndA`, both rules give the same keys: `first-name`,
 * `my-value123` and `instant-id-qand-a`. Use it to keep keys that were written for the older rule.
 *
 * It applies to the class's own fields, not to the fields of a nested case class (annotate that class too), and not to
 * a field with `@name`. Use one key-case annotation per class: with several, Scala 2 uses the first one, and Scala 3
 * prefers `@kebabCase`, then `@kebabCaseLegacy`, then `@snakeCase`.
 *
 * {{{
 * import zio._
 * import zio.config.magnolia.deriveConfig
 *
 * @kebabCaseLegacy
 * final case class Api(version2Name: String, firstName: String)
 *
 * val api: IO[Config.Error, Api] =
 *   ConfigProvider.fromMap(Map("version2name" -> "v2", "first-name" -> "orders")).load(deriveConfig[Api])
 * // succeeds with Api(v2,orders)
 * }}}
 */
final case class kebabCaseLegacy() extends StaticAnnotation

/**
 * Converts the field names of the annotated case class to snake_case keys: `firstName` becomes `first_name`,
 * `version2Name` becomes `version2_name`, and `URLPath` becomes `urlpath`.
 *
 * It applies to the class's own fields, not to the fields of a nested case class (annotate that class too), and not to
 * a field with `@name`. Use one key-case annotation per class: with several, Scala 2 uses the first one, and Scala 3
 * prefers `@kebabCase`, then `@kebabCaseLegacy`, then `@snakeCase`.
 *
 * {{{
 * import zio._
 * import zio.config.magnolia.deriveConfig
 *
 * @snakeCase
 * final case class Account(userName: String, maxSessions: Int)
 *
 * val account: IO[Config.Error, Account] =
 *   ConfigProvider.fromMap(Map("user_name" -> "ada", "max_sessions" -> "3")).load(deriveConfig[Account])
 * // succeeds with Account(ada,3)
 * }}}
 */
final case class snakeCase() extends StaticAnnotation

/**
 * Adds `prefix` before each field name of the annotated case class, and capitalizes the field name: with
 * `@prefix("db")`, `poolSize` becomes `dbPoolSize`. It is applied before a key-case annotation, so
 * `@prefix("db") @kebabCase` gives `db-pool-size`.
 *
 * It applies to the class's own fields, is ignored on a field, and doesn't change a field with `@name`. With more than
 * one `@prefix` on a class, Scala 2 and Scala 3 apply them in different orders.
 *
 * {{{
 * import zio._
 * import zio.config.magnolia.deriveConfig
 *
 * @prefix("db") @kebabCase
 * final case class Pool(poolSize: Int, timeoutSeconds: Int)
 *
 * val pool: IO[Config.Error, Pool] =
 *   ConfigProvider.fromMap(Map("db-pool-size" -> "10", "db-timeout-seconds" -> "5")).load(deriveConfig[Pool])
 * // succeeds with Pool(10,5)
 * }}}
 *
 * @param prefix
 *   the text to add before each field name
 */
final case class prefix(prefix: String) extends StaticAnnotation

/**
 * Adds `postfix`, capitalized, after each field name of the annotated case class: with `@postfix("ms")`, `timeout`
 * becomes `timeoutMs`. It is applied before a key-case annotation, so `@postfix("ms") @snakeCase` gives `timeout_ms`.
 *
 * It applies to the class's own fields, is ignored on a field, and doesn't change a field with `@name`. With more than
 * one `@postfix` on a class, Scala 2 and Scala 3 apply them in different orders.
 *
 * {{{
 * import zio._
 * import zio.config.magnolia.deriveConfig
 *
 * @postfix("ms")
 * final case class Timeouts(connect: Long, read: Long)
 *
 * val timeouts: IO[Config.Error, Timeouts] =
 *   ConfigProvider.fromMap(Map("connectMs" -> "500", "readMs" -> "2000")).load(deriveConfig[Timeouts])
 * // succeeds with Timeouts(500,2000)
 * }}}
 *
 * @param postfix
 *   the text to add after each field name
 */
final case class postfix(postfix: String) extends StaticAnnotation
