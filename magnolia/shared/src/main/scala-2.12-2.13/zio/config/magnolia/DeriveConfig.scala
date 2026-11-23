package zio.config.magnolia

import magnolia._
import zio.config._
import zio.{Chunk, Config, LogLevel, NonEmptyChunk}

import java.net.URI
import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime}
import java.util.UUID
import scala.collection.immutable

/**
 * The type class behind derivation: a `DeriveConfig[A]` holds the `Config[A]` description that `deriveConfig[A]`
 * returns. ZIO Config provides instances for common types (`String`, `Int`, `zio.Duration`, `List[A]`, `Map[String, A]`
 * and others) and derives instances for case classes and sealed traits at compile time.
 *
 * To read a type that derivation can't handle, transform an existing instance with `map`, `mapAttempt` or `mapOrFail`,
 * or wrap a hand-written description with `DeriveConfig(config)`, and put the result in implicit scope.
 *
 * Derivation also stores data that it uses to read sealed traits: on Scala 2, whether the type is a case object or a
 * case class without fields (`isObject`) and a case object's value (`constValue`); on Scala 3, its names (`metadata`).
 * An instance built by hand, or returned by `map`, `describe` or the other methods, has none of it. A subtype of a
 * sealed trait with such an instance is read differently on Scala 2 and Scala 3, and on Scala 3 `@discriminator` can't
 * select it.
 *
 * {{{
 * import java.nio.file.{Path, Paths}
 * import zio._
 * import zio.config.magnolia._
 *
 * implicit val pathConfig: DeriveConfig[Path] =
 *   DeriveConfig[String].mapAttempt(Paths.get(_)) ?? "a file system path"
 *
 * final case class Storage(dataDir: Path, maxFiles: Int)
 *
 * val storage: IO[Config.Error, Storage] =
 *   ConfigProvider.fromMap(Map("dataDir" -> "/var/lib/app", "maxFiles" -> "100")).load(deriveConfig[Storage])
 * // succeeds with Storage(/var/lib/app,100)
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-config/automatic-derivation-of-config/ Automatic Derivation of Config]]
 */
final case class DeriveConfig[T](desc: Config[T], isObject: Boolean = false, constValue: Option[T] = None) {

  /** Same as `describe`: returns an instance whose description has `description` added, for `generateDocs`. */
  def ??(description: String): DeriveConfig[T] =
    describe(description)

  /**
   * Returns an instance whose description has `description` added, as `Config#??` does. The text appears in the output
   * of `generateDocs`; the keys that are read don't change.
   *
   * {{{
   * import zio.config.magnolia._
   *
   * val port: DeriveConfig[Int] = DeriveConfig[Int].describe("Port the server listens on")
   * }}}
   */
  def describe(description: String): DeriveConfig[T] =
    DeriveConfig(desc.??(description))

  /**
   * Returns an instance that reads the value as before and converts it with `f`. Use it when the conversion can't fail.
   *
   * {{{
   * import zio.config.magnolia._
   *
   * final case class Port(value: Int)
   *
   * implicit val portConfig: DeriveConfig[Port] = DeriveConfig[Int].map(Port(_))
   * }}}
   */
  def map[B](f: T => B): DeriveConfig[B] =
    DeriveConfig(desc.map(f))

  /**
   * Like `map`, but an exception that `f` throws fails reading with a `Config.Error.InvalidData` whose message is the
   * exception's message.
   *
   * {{{
   * import java.util.Currency
   * import zio._
   * import zio.config.magnolia._
   *
   * implicit val currencyConfig: DeriveConfig[Currency] = DeriveConfig[String].mapAttempt(Currency.getInstance(_))
   *
   * final case class Pricing(currency: Currency)
   *
   * val euro: IO[Config.Error, Pricing] =
   *   ConfigProvider.fromMap(Map("currency" -> "EUR")).load(deriveConfig[Pricing])
   * // succeeds with Pricing(EUR)
   *
   * val unknown: IO[Config.Error, Pricing] =
   *   ConfigProvider.fromMap(Map("currency" -> "XXXX")).load(deriveConfig[Pricing])
   * // fails with an InvalidData error at `currency`
   * }}}
   */
  def mapAttempt[B](f: T => B): DeriveConfig[B] =
    DeriveConfig(desc.mapAttempt(f))

  /**
   * Returns an instance that converts the value with `f`, and fails reading with the `Config.Error` that `f` returns in
   * a `Left`.
   *
   * {{{
   * import zio._
   * import zio.config.magnolia._
   *
   * final case class Percent(value: Int)
   *
   * implicit val percentConfig: DeriveConfig[Percent] =
   *   DeriveConfig[Int].mapOrFail { n =>
   *     if (n >= 0 && n <= 100) Right(Percent(n))
   *     else Left(Config.Error.InvalidData(message = "expected 0 to 100, got " + n))
   *   }
   * }}}
   */
  def mapOrFail[B](f: T => Either[Config.Error, B]): DeriveConfig[B] =
    DeriveConfig(desc.mapOrFail(f))

}

/** Instances of `DeriveConfig` for common types, and the derivation of instances for case classes and sealed traits. */
object DeriveConfig {

  /**
   * Returns the `DeriveConfig[A]` that implicit search finds: an instance in scope, a built-in one, or one derived for
   * a case class or sealed trait.
   */
  def apply[A](implicit ev: DeriveConfig[A]): DeriveConfig[A] =
    ev

  import Config._

  /** Reads a text value as a `Config.Secret`, with `Config.secret`. */
  implicit val implicitSecretDesc: DeriveConfig[Secret] = DeriveConfig(secret)

  /**
   * Reads an ISO-8601 date-time with an offset, such as `2024-01-02T03:04:05+01:00` or `2024-01-02T03:04:05Z`, with
   * `Config.offsetDateTime`. A value without an offset fails.
   */
  implicit val implicitOffsetDateTimeDesc: DeriveConfig[OffsetDateTime] = DeriveConfig(offsetDateTime)

  /**
   * Reads a `zio.LogLevel` by its name, with `Config.logLevel`, such as `INFO`, `WARN` or `ERROR` (`info` works too).
   */
  implicit val implicitLogLevelDesc: DeriveConfig[LogLevel] = DeriveConfig(logLevel)

  /** Reads a text value as it is, with `Config.string`. */
  implicit val implicitStringDesc: DeriveConfig[String] = DeriveConfig(string)

  /**
   * Reads `true`, `yes`, `on` or `1` as `true`, and `false`, `no`, `off` or `0` as `false`, with `Config.boolean`.
   * Upper case, such as `TRUE`, works too.
   */
  implicit val implicitBooleanDesc: DeriveConfig[Boolean] = DeriveConfig(boolean)

  /** Reads an `Int` written in decimal, such as `42`, with `Config.int`. */
  implicit val implicitIntDesc: DeriveConfig[Int] = DeriveConfig(int)

  /** Reads a `BigInt` written in decimal, such as `123456789012345678901234567890`, with `Config.bigInt`. */
  implicit val implicitBigIntDesc: DeriveConfig[BigInt] = DeriveConfig(bigInt)

  /** Reads a `Float` such as `1.5` or `1e3`, with `Config.float`. `NaN` and `Infinity` fail. */
  implicit val implicitFloatDesc: DeriveConfig[Float] = DeriveConfig(float)

  /** Reads a `Double` such as `1.5` or `1e3`, with `Config.double`. `NaN` and `Infinity` fail. */
  implicit val implicitDoubleDesc: DeriveConfig[Double] = DeriveConfig(double)

  /** Reads a `BigDecimal` such as `1.50` or `1e-3`, with `Config.bigDecimal`. */
  implicit val implicitBigDecimalDesc: DeriveConfig[BigDecimal] = DeriveConfig(bigDecimal)

  /** Reads a `java.net.URI`, with `Config.uri`. Text that isn't a valid URI fails with the parser's message. */
  implicit val implicitUriDesc: DeriveConfig[URI] = DeriveConfig(uri)

  /**
   * Reads a `zio.Duration`, with `Config.duration`: ISO-8601 text such as `PT5S`, or text such as `5 seconds`,
   * `100 millis` or `1h`. A bare number such as `5000` fails.
   */
  implicit val implicitDurationDesc: DeriveConfig[zio.Duration] = DeriveConfig(duration)

  /** Reads an ISO-8601 date such as `2024-01-02`, with `Config.localDate`. */
  implicit val implicitLocalDateDesc: DeriveConfig[LocalDate] = DeriveConfig(localDate)

  /** Reads an ISO-8601 time such as `03:04` or `03:04:05.123`, with `Config.localTime`. */
  implicit val implicitLocalTimeDesc: DeriveConfig[LocalTime] = DeriveConfig(localTime)

  /**
   * Reads an ISO-8601 date-time such as `2024-01-02T03:04:05`, with `Config.localDateTime`. A space in place of the `T`
   * fails.
   */
  implicit val implicitLocalDateTimeDesc: DeriveConfig[LocalDateTime] = DeriveConfig(localDateTime)

  /** Reads a `Byte`, with `Config.byte`. A value out of range, such as `128`, fails. */
  implicit val implicitByteDesc: DeriveConfig[Byte] = DeriveConfig(Config.byte)

  /** Reads a `Short`, with `Config.short`. A value out of range, such as `32768`, fails. */
  implicit val implicitShortDesc: DeriveConfig[Short] = DeriveConfig(Config.short)

  /**
   * Reads a `java.util.UUID` in its standard text form, such as `123e4567-e89b-12d3-a456-426614174000`, with
   * `Config.uuid`.
   */
  implicit val implicitUUIDDesc: DeriveConfig[UUID] = DeriveConfig(Config.uuid)

  /** Reads a `Long` written in decimal, with `Config.long`. */
  implicit val implicitLongDesc: DeriveConfig[Long] = DeriveConfig(Config.long)

  /** Makes a value optional: a missing key gives `None`. A value that is present but can't be parsed still fails. */
  implicit def implicitOptionDesc[A: DeriveConfig]: DeriveConfig[Option[A]] =
    DeriveConfig(DeriveConfig[A].desc.optional)

  /**
   * Reads an `A`, and when that fails, a `B` (`Config#orElseEither`): with `Either[Int, String]`, `3` gives `Left(3)`
   * and `abc` gives `Right(abc)`.
   */
  implicit def implicitEitherDesc[A: DeriveConfig, B: DeriveConfig]: DeriveConfig[Either[A, B]] =
    DeriveConfig(DeriveConfig[A].desc.orElseEither(DeriveConfig[B].desc))

  /**
   * Reads a `List[A]`, with `Config.listOf`. With `ConfigProvider.fromMap`, a list is one value split on `,` by
   * default, such as `1,2,3`.
   */
  implicit def implicitListDesc[A: DeriveConfig]: DeriveConfig[List[A]] =
    DeriveConfig(Config.listOf(implicitly[DeriveConfig[A]].desc))

  /** Reads a `Seq[A]` as a `List[A]`, with `Config.listOf`. */
  implicit def implicitListSeq[A: DeriveConfig]: DeriveConfig[Seq[A]] =
    DeriveConfig(Config.listOf(implicitly[DeriveConfig[A]].desc).map(_.toSeq))

  /** Reads a `Set[A]`, with `Config.setOf`. Duplicate elements are dropped. */
  implicit def implicitSetDesc[A: DeriveConfig]: DeriveConfig[Set[A]] =
    DeriveConfig(Config.setOf(implicitly[DeriveConfig[A]].desc))

  /** Reads a `Vector[A]`, with `Config.vectorOf`. */
  implicit def implicitVectorDesc[A: DeriveConfig]: DeriveConfig[Vector[A]] =
    DeriveConfig(Config.vectorOf(DeriveConfig[A].desc))

  /** Reads a `Chunk[A]`, with `Config.chunkOf`. */
  implicit def implicitChunkDesc[A: DeriveConfig]: DeriveConfig[Chunk[A]] =
    DeriveConfig(Config.chunkOf(DeriveConfig[A].desc))

  /** Reads a `NonEmptyChunk[A]`, with `Config.nonEmptyChunkOf`. */
  implicit def implicitNonEmptyChunkDesc[A: DeriveConfig]: DeriveConfig[NonEmptyChunk[A]] =
    DeriveConfig(Config.nonEmptyChunkOf(DeriveConfig[A].desc))

  /**
   * Reads the keys directly under the field as the keys of a `Map[String, A]`, with `Config.table`, and reads each
   * value with the instance for `A`. When no key is set, the map is empty.
   */
  implicit def implicitMapDesc[A: DeriveConfig]: DeriveConfig[Map[String, A]] =
    DeriveConfig(Config.table(implicitly[DeriveConfig[A]].desc))

  /** The type class that Magnolia derives: `DeriveConfig`. */
  type Typeclass[T] = DeriveConfig[T]

  /** A change that derivation makes to a field name: a prefix, a postfix or a key case. */
  sealed trait KeyModifier

  /** A key-case change, from `@kebabCase`, `@kebabCaseLegacy` or `@snakeCase`, or no change. */
  sealed trait CaseModifier extends KeyModifier

  /** The key modifiers, and the function that each one applies. */
  object KeyModifier {

    /** The change for `@kebabCase`: `firstName` becomes `first-name`. */
    case object KebabCase extends CaseModifier

    /** The change for `@kebabCaseLegacy`: `version2Name` becomes `version2name`. */
    case object KebabCaseLegacy extends CaseModifier

    /** The change for `@snakeCase`: `firstName` becomes `first_name`. */
    case object SnakeCase extends CaseModifier

    /** No key-case annotation: the field name is kept. */
    case object NoneModifier extends CaseModifier

    /** The change for `@prefix`: `Prefix("db")` turns `poolSize` into `dbPoolSize`. */
    case class Prefix(prefix: String) extends KeyModifier

    /** The change for `@postfix`: `Postfix("ms")` turns `timeout` into `timeoutMs`. */
    case class Postfix(postfix: String) extends KeyModifier

    /** Returns the function that applies `keyModifier` to a field name. */
    def getModifierFunction(keyModifier: KeyModifier): String => String =
      keyModifier match {
        case KebabCase        => toKebabCase
        case KebabCaseLegacy  => toKebabCaseLegacy
        case SnakeCase        => toSnakeCase
        case Prefix(prefix)   => addPrefixToKey(prefix)
        case Postfix(postfix) => addPostFixToKey(postfix)
        case NoneModifier     => identity
      }
  }

  /**
   * Reads `desc` under the first of `labels`, or returns `desc` unchanged when `labels` is empty. Derivation uses it
   * for `@name` on a sealed trait.
   */
  final def wrapSealedTrait[T](
    labels: Seq[String],
    desc: Config[T]
  ): Config[T] = {
    val f = (name: String) => desc.nested(name)
    labels.toList match {
      case head :: _     => f(head)
      case immutable.Nil => desc
      case multiple      =>
        multiple.tail.foldLeft(f(multiple.head)) { case (acc, n) =>
          acc orElse f(n)
        }
    }

  }

  /** Returns the value of the first `@name` in `annotations`, or `defaultClassName` when there is none. */
  final def prepareClassName(annotations: Seq[Any], defaultClassName: String): String =
    annotations.collectFirst { case d: name => d.name }.getOrElse(defaultClassName)

  /** Returns the value of the first `@name` in `annotations`, if there is one. */
  final def prepareSealedTraitName(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case d: name => d.name }

  /**
   * Returns the key for a field: the value of its `@name` when it has one, otherwise `name` with `keyModifiers` applied
   * in order and then `caseModifier`.
   */
  final def prepareFieldName(
    annotations: Seq[Any],
    name: String,
    keyModifiers: List[KeyModifier],
    caseModifier: CaseModifier
  ): String =
    annotations.collectFirst { case d: name => d.name }.getOrElse {
      val modifyKey = keyModifiers
        .foldLeft(identity[String] _) { case (allModifications, keyModifier) =>
          allModifications.andThen(KeyModifier.getModifierFunction(keyModifier))
        }
        .andThen(KeyModifier.getModifierFunction(caseModifier))
      modifyKey(name)
    }

  /**
   * Returns the `@prefix` and `@postfix` modifiers in `annotations`, in order, and the first key-case annotation
   * (`NoneModifier` when there is none).
   */
  final def checkKeyModifier(annotations: Seq[Any]): (List[KeyModifier], CaseModifier) = {
    val modifiers = annotations.collect {
      case p: prefix  => KeyModifier.Prefix(p.prefix)
      case p: postfix => KeyModifier.Postfix(p.postfix)
    }.toList

    val caseModifier = annotations.collectFirst {
      case _: kebabCase       => KeyModifier.KebabCase
      case _: kebabCaseLegacy => KeyModifier.KebabCaseLegacy
      case _: snakeCase       => KeyModifier.SnakeCase
    }.getOrElse(KeyModifier.NoneModifier)
    modifiers -> caseModifier
  }

  /**
   * Magnolia's step for a case class. Each field is read under its key, with its `@describe` texts and its default
   * value. The class's `@describe` texts are added to the whole description, so `generateDocs` shows them on each
   * field. A case object or a case class without fields is read as its `@name`, or as its name when it has none. A
   * value class is read as its single field, without a key of its own.
   */
  final def combine[T](caseClass: CaseClass[DeriveConfig, T]): DeriveConfig[T] = {
    val descriptions                 = caseClass.annotations.collect { case d: describe => d.describe }
    val ccName                       = prepareClassName(caseClass.annotations, caseClass.typeName.short)
    val (keyModifiers, caseModifier) = checkKeyModifier(caseClass.annotations)

    val res =
      caseClass.parameters.toList match {
        case Nil =>
          Config
            .constant(ccName)
            .map(_ => caseClass.rawConstruct(Seq.empty))

        case head :: tail =>
          def nest(name: String)(unwrapped: Config[Any]) =
            if (caseClass.isValueClass) unwrapped
            else unwrapped.nested(name)

          def makeDescriptor(param: Param[DeriveConfig, T]): Config[Any] = {
            val descriptions =
              param.annotations
                .filter(_.isInstanceOf[describe])
                .map(_.asInstanceOf[describe].describe)

            val raw         = param.typeclass.desc
            val withNesting = nest(prepareFieldName(param.annotations, param.label, keyModifiers, caseModifier))(raw)

            val described = descriptions.foldLeft(withNesting)(_ ?? _)
            param.default.fold(described)(described.withDefault(_))
          }

          Config
            .collectAll(
              Config.defer(makeDescriptor(head)),
              tail.map(a => Config.defer(makeDescriptor(a))): _*
            )
            .map[T](l => caseClass.rawConstruct(l))
      }

    val constValue =
      if (caseClass.isObject) Some(caseClass.rawConstruct(Seq.empty))
      else None

    DeriveConfig(
      descriptions.foldLeft(res)(_ ?? _),
      caseClass.isObject || caseClass.parameters.isEmpty,
      constValue
    )
  }

  /**
   * Magnolia's step for a sealed trait. Without `@discriminator`, each subtype is tried in turn: a case object as its
   * name, a case class under a key with its name. Case classes with the same short name are read under `Name_0`,
   * `Name_1` and so on. With `@discriminator`, the subtype's name is read from that key. `@name` on the sealed trait
   * reads all of it under one more key.
   */
  final def dispatch[T](sealedTrait: SealedTrait[DeriveConfig, T]): DeriveConfig[T] = {
    val nameToLabel =
      sealedTrait.subtypes
        .map(tc => prepareClassName(tc.annotations, tc.typeName.short) -> tc.typeName.full)
        .groupBy(_._1)
        .toSeq
        .flatMap {
          case (label, Seq((_, fullName))) => (fullName -> label) :: Nil
          case (label, seq)                =>
            seq.zipWithIndex.map { case ((_, fullName), idx) => fullName -> s"${label}_$idx" }
        }
        .toMap

    val keyNameIfPureConfig: Option[String] =
      sealedTrait.annotations.collectFirst { case d: discriminator => d.keyName }

    val desc =
      keyNameIfPureConfig match {
        case None =>
          sealedTrait.subtypes.map { subtype =>
            val typeclass: DeriveConfig[subtype.SType] = subtype.typeclass

            val subClassName =
              nameToLabel(subtype.typeName.full)

            if (typeclass.isObject)
              typeclass.desc
            else
              typeclass.desc.nested(subClassName)
          }.reduce(_.orElse(_))

        case Some(pureConfigKeyName) =>
          Config
            .string(pureConfigKeyName)
            .switch(
              sealedTrait.subtypes.map { subtype =>
                val desc =
                  subtype.typeclass.constValue match {
                    case Some(v) => Config.Constant(v)
                    case None    => subtype.typeclass.desc
                  }
                nameToLabel(subtype.typeName.full) -> desc
              }: _*
            )
      }

    DeriveConfig(
      prepareSealedTraitName(sealedTrait.annotations).fold(desc)(name => wrapSealedTrait(List(name), desc))
    )
  }

  /**
   * Derives a `DeriveConfig[T]` for a case class or sealed trait with Magnolia. Implicit search finds it when no other
   * instance for `T` is in scope.
   */
  implicit def getDeriveConfig[T]: DeriveConfig[T] = macro Magnolia.gen[T]

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

}
