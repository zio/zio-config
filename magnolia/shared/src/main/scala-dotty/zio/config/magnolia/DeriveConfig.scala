package zio.config.magnolia

import zio.Config.*
import zio.config.*
import zio.config.derivation.*
import zio.config.magnolia.DeriveConfig.*
import zio.{Chunk, Config, LogLevel, NonEmptyChunk}

import java.net.URI
import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime, *}
import java.util.UUID
import scala.annotation.{targetName, threadUnsafe}
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

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
final case class DeriveConfig[A](desc: Config[A], metadata: Option[DeriveConfig.Metadata] = None) {

  /** Same as `describe`: returns an instance whose description has `description` added, for `generateDocs`. */
  def ??(description: String): DeriveConfig[A] =
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
  def describe(description: String): DeriveConfig[A] =
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
  def map[B](f: A => B): DeriveConfig[B] =
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
  def mapAttempt[B](f: A => B): DeriveConfig[B] =
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
  def mapOrFail[B](f: A => Either[Config.Error, B]): DeriveConfig[B] =
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

  /** Wraps `desc` as an instance without derivation metadata; the same as `DeriveConfig(desc)`. */
  def from[A](desc: Config[A]) =
    DeriveConfig(desc, None)

  /** What derivation records about a type to read sealed traits: its names, and for a case object its value. */
  sealed trait Metadata {

    /** The Scala name of the type, without `@name`. */
    def originalName: String = this match {
      case Metadata.Object(name, _)    => name.originalName
      case Metadata.Product(name, _)   => name.originalName
      case Metadata.Coproduct(name, _) => name.originalName
    }

    /** The values of the `@name` annotations on the type; empty for a case object. */
    def alternativeNames: List[String] = this match {
      case Metadata.Object(_, _)       => Nil
      case Metadata.Product(name, _)   => name.alternativeNames
      case Metadata.Coproduct(name, _) => name.alternativeNames
    }
  }

  /** The kinds of `Metadata`. */
  object Metadata {

    /** A case object, or a case class without fields, with its value. */
    final case class Object[T](name: ProductName, constValue: T) extends Metadata

    /** A case class with fields, with the names of its fields. */
    final case class Product(name: ProductName, fields: List[FieldName]) extends Metadata

    /** A sealed trait, with the metadata of its subtypes. */
    final case class Coproduct(name: CoproductName, metadata: List[Metadata]) extends Metadata
  }

  /**
   * The names of a field: the key that derivation reads (after `@name` and the key annotations), the value of its
   * `@name`, and its `@describe` texts.
   */
  final case class FieldName(originalName: String, alternativeNames: List[String], descriptions: List[String])

  /** The names of a case class: its Scala name, the values of its `@name` annotations, and its `@describe` texts. */
  final case class ProductName(originalName: String, alternativeNames: List[String], descriptions: List[String])

  /**
   * The names of a sealed trait: its Scala name, the values of its `@name` annotations, its `@describe` texts, and the
   * key of its `@discriminator`.
   */
  final case class CoproductName(
    originalName: String,
    alternativeNames: List[String],
    descriptions: List[String],
    typeDiscriminator: Option[String]
  )

  /** Reads a text value as a `Config.Secret`, with `Config.secret`. */
  given DeriveConfig[Secret] = DeriveConfig.from(secret)

  /**
   * Reads an ISO-8601 date-time with an offset, such as `2024-01-02T03:04:05+01:00` or `2024-01-02T03:04:05Z`, with
   * `Config.offsetDateTime`. A value without an offset fails.
   */
  given DeriveConfig[OffsetDateTime] = DeriveConfig.from(offsetDateTime)

  /**
   * Reads a `zio.LogLevel` by its name, with `Config.logLevel`, such as `INFO`, `WARN` or `ERROR` (`info` works too).
   */
  given DeriveConfig[LogLevel] = DeriveConfig.from(logLevel)

  /** Reads a text value as it is, with `Config.string`. */
  given DeriveConfig[String] = DeriveConfig.from(string)

  /**
   * Reads `true`, `yes`, `on` or `1` as `true`, and `false`, `no`, `off` or `0` as `false`, with `Config.boolean`.
   * Upper case, such as `TRUE`, works too.
   */
  given DeriveConfig[Boolean] = DeriveConfig.from(boolean)

  /** Reads an `Int` written in decimal, such as `42`, with `Config.int`. */
  given DeriveConfig[Int] = DeriveConfig.from(int)

  /** Reads a `BigInt` written in decimal, such as `123456789012345678901234567890`, with `Config.bigInt`. */
  given DeriveConfig[BigInt] = DeriveConfig.from(bigInt)

  /** Reads a `Float` such as `1.5` or `1e3`, with `Config.float`. `NaN` and `Infinity` fail. */
  given DeriveConfig[Float] = DeriveConfig.from(float)

  /** Reads a `Double` such as `1.5` or `1e3`, with `Config.double`. `NaN` and `Infinity` fail. */
  given DeriveConfig[Double] = DeriveConfig.from(double)

  /** Reads a `BigDecimal` such as `1.50` or `1e-3`, with `Config.bigDecimal`. */
  given DeriveConfig[BigDecimal] = DeriveConfig.from(bigDecimal)

  /** Reads a `java.net.URI`, with `Config.uri`. Text that isn't a valid URI fails with the parser's message. */
  given DeriveConfig[URI] = DeriveConfig.from(uri)

  /**
   * Reads a `zio.Duration`, with `Config.duration`: ISO-8601 text such as `PT5S`, or text such as `5 seconds`,
   * `100 millis` or `1h`. A bare number such as `5000` fails.
   */
  given DeriveConfig[zio.Duration] = DeriveConfig.from(duration)

  /** Reads an ISO-8601 date such as `2024-01-02`, with `Config.localDate`. */
  given DeriveConfig[LocalDate] = DeriveConfig.from(localDate)

  /** Reads an ISO-8601 time such as `03:04` or `03:04:05.123`, with `Config.localTime`. */
  given DeriveConfig[LocalTime] = DeriveConfig.from(localTime)

  /**
   * Reads an ISO-8601 date-time such as `2024-01-02T03:04:05`, with `Config.localDateTime`. A space in place of the `T`
   * fails.
   */
  given DeriveConfig[LocalDateTime] = DeriveConfig.from(localDateTime)

  /** Reads a `Byte`, with `Config.byte`. A value out of range, such as `128`, fails. */
  given DeriveConfig[Byte] = DeriveConfig(Config.byte)

  /** Reads a `Short`, with `Config.short`. A value out of range, such as `32768`, fails. */
  given DeriveConfig[Short] = DeriveConfig(Config.short)

  /**
   * Reads a `java.util.UUID` in its standard text form, such as `123e4567-e89b-12d3-a456-426614174000`, with
   * `Config.uuid`.
   */
  given DeriveConfig[UUID] = DeriveConfig(Config.uuid)

  /** Reads a `Long` written in decimal, with `Config.long`. */
  given DeriveConfig[Long] = DeriveConfig(Config.long)

  /** Makes a value optional: a missing key gives `None`. A value that is present but can't be parsed still fails. */
  given optDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Option[A]] =
    DeriveConfig.from(ev.desc.optional)

  /**
   * Reads an `A`, and when that fails, a `B` (`Config#orElseEither`): with `Either[Int, String]`, `3` gives `Left(3)`
   * and `abc` gives `Right(abc)`.
   */
  given eitherConfig[A, B](using evA: DeriveConfig[A], evB: DeriveConfig[B]): DeriveConfig[Either[A, B]] =
    DeriveConfig.from(evA.desc.orElseEither(evB.desc))

  /**
   * Reads a `List[A]`, with `Config.listOf`. With `ConfigProvider.fromMap`, a list is one value split on `,` by
   * default, such as `1,2,3`.
   */
  given listDesc[A](using ev: DeriveConfig[A]): DeriveConfig[List[A]] =
    DeriveConfig.from(listOf(ev.desc))

  /** Reads a `Seq[A]` as a `List[A]`, with `Config.listOf`. */
  given seqDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Seq[A]] =
    DeriveConfig.from(listOf(ev.desc).map(_.toSeq))

  /** Reads a `Set[A]`, with `Config.setOf`. Duplicate elements are dropped. */
  given setDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Set[A]] =
    DeriveConfig.from(setOf(ev.desc))

  /** Reads a `Vector[A]`, with `Config.vectorOf`. */
  given vectorDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Vector[A]] =
    DeriveConfig.from(vectorOf(ev.desc))

  /** Reads a `Chunk[A]`, with `Config.chunkOf`. */
  given chunkDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Chunk[A]] =
    DeriveConfig.from(chunkOf(ev.desc))

  /** Reads a `NonEmptyChunk[A]`, with `Config.nonEmptyChunkOf`. */
  given nonEmptyChunkDesc[A](using ev: DeriveConfig[A]): DeriveConfig[NonEmptyChunk[A]] =
    DeriveConfig.from(nonEmptyChunkOf(ev.desc))

  /**
   * Reads the keys directly under the field as the keys of a `Map[String, A]`, with `Config.table`, and reads each
   * value with the instance for `A`. When no key is set, the map is empty.
   */
  given mapDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Map[String, A]] =
    DeriveConfig.from(table(ev.desc))

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
    final case class Prefix(prefix: String) extends KeyModifier

    /** The change for `@postfix`: `Postfix("ms")` turns `timeout` into `timeoutMs`. */
    final case class Postfix(postfix: String) extends KeyModifier

    /** Returns the function that applies `keyModifier` to a field name. */
    def modifierFunction(keyModifier: KeyModifier): String => String =
      keyModifier match
        case KebabCase        => toKebabCase
        case KebabCaseLegacy  => toKebabCaseLegacy
        case SnakeCase        => toSnakeCase
        case Prefix(prefix)   => addPrefixToKey(prefix)
        case Postfix(postfix) => addPostFixToKey(postfix)
        case NoneModifier     => identity
  }

  /** Summons the instance for each type in the tuple `T`, typed as `DeriveConfig[Any]`. */
  inline def summonDeriveConfigForCoProduct[T <: Tuple]: List[DeriveConfig[Any]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  =>
        val desc = summonInline[DeriveConfig[t]]
        DeriveConfig[Any](
          desc.desc,
          desc.metadata
        ) :: summonDeriveConfigForCoProduct[ts]
    }

  /** Summons the instance for each type in the tuple `T`. */
  inline def summonDeriveConfigAll[T <: Tuple]: List[DeriveConfig[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[DeriveConfig[t]] :: summonDeriveConfigAll[ts]
    }

  /** Returns the values of the literal types in the tuple `T`, such as the field names of a mirror. */
  inline def labelsOf[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].toString :: labelsOf[ts]
    }

  /** Returns the values of the `@name` annotations on `T`. */
  inline def customNamesOf[T]: List[String] =
    inline Macros.nameOf[T] match {
      case Nil   => Nil
      case names => names.map(_.name)
    }

  /** Returns the `@name` annotation of each field of `T` that has one, by field name. */
  inline def customFieldNamesOf[T]: Map[String, name] =
    inline Macros.fieldNameOf[T] match {
      case Nil   => Map.empty[String, name]
      case names => names.flatMap { case (str, nmes) => nmes.map(name => (str, name)) }.toMap
    }

  /** Returns the `@prefix` and `@postfix` modifiers of `T`, and its key-case modifier. */
  inline def keyModifiersOf[T]: (List[KeyModifier], CaseModifier) =
    ${ keyModifiersOfImpl[T] }

  /**
   * Derives a `DeriveConfig[T]` for a case class or sealed trait from its `Mirror`. Implicit search finds it when no
   * other instance for `T` is in scope; `deriveConfig` describes the rules.
   */
  inline given derived[T](using m: Mirror.Of[T]): DeriveConfig[T] =
    inline m match
      case _: Mirror.SumOf[T] =>
        val coproductName: CoproductName =
          CoproductName(
            originalName = constValue[m.MirroredLabel],
            alternativeNames = customNamesOf[T],
            descriptions = Macros.documentationOf[T].map(_.describe),
            typeDiscriminator = Macros.discriminator[T].headOption.map(_.keyName)
          )

        val subClassDescriptions = summonDeriveConfigForCoProduct[m.MirroredElemTypes]
        val desc                 = mergeAllProducts(subClassDescriptions.map(castTo[DeriveConfig[T]]), coproductName.typeDiscriminator)

        DeriveConfig.from(tryAllKeys(desc.desc, None, coproductName.alternativeNames))

      case m: Mirror.ProductOf[T] =>
        val productName =
          ProductName(
            originalName = constValue[m.MirroredLabel],
            alternativeNames = customNamesOf[T],
            descriptions = Macros.documentationOf[T].map(_.describe)
          )

        val originalFieldNamesList       = labelsOf[m.MirroredElemLabels]
        val customFieldNameMap           = customFieldNamesOf[T]
        val documentations               = Macros.fieldDocumentationOf[T].toMap
        val (keyModifiers, caseModifier) = keyModifiersOf[T]
        val fieldNames                   =
          mapOriginalNames(originalFieldNamesList, documentations, customFieldNameMap, keyModifiers, caseModifier)

        @threadUnsafe lazy val fieldConfigsWithDefaultValues = {
          val fieldConfigs          = summonDeriveConfigAll[m.MirroredElemTypes].asInstanceOf[List[DeriveConfig[Any]]]
          val fieldAndDefaultValues = Macros.defaultValuesOf[T].toMap
          addDefaultValues(fieldAndDefaultValues, originalFieldNamesList, fieldConfigs)
        }

        mergeAllFields(
          fieldConfigsWithDefaultValues,
          productName,
          fieldNames,
          lst => m.fromProduct(Tuple.fromArray(lst.toArray[Any]))
        )

  private def mapOriginalNames(
    names: List[String],
    docs: Map[String, List[describe]],
    customFieldNames: Map[String, name],
    keyModifiers: List[KeyModifier],
    caseModifier: CaseModifier
  ): List[FieldName] =
    names.foldRight(List.empty[FieldName]) { (str, list) =>
      val alternativeNames = customFieldNames.get(str).map(v => List(v.name)).getOrElse(Nil)
      val descriptions     = docs.get(str).map(_.map(_.describe)).getOrElse(Nil)
      val modifyKey        = keyModifiers
        .foldLeft(identity[String] _) { case (all, modifier) =>
          all.andThen(KeyModifier.modifierFunction(modifier))
        }
        .andThen(KeyModifier.modifierFunction(caseModifier))
      val targetName       = customFieldNames.get(str).map(_.name).getOrElse(modifyKey(str))

      FieldName(targetName, alternativeNames, descriptions) :: list
    }

  /**
   * Combines the instances of the subtypes of a sealed trait. Without `typeDiscriminator`, each subtype is tried in
   * turn, a case class with fields under its `@name` values, or under its name when it has none. With
   * `typeDiscriminator`, the subtype's name is read from that key; a subtype whose instance has no metadata is left
   * out.
   */
  def mergeAllProducts[T](
    allDescs: => List[DeriveConfig[T]],
    typeDiscriminator: Option[String]
  ): DeriveConfig[T] = {
    val desc =
      typeDiscriminator match {
        case None =>
          allDescs
            .map(desc =>
              desc.metadata match {
                case Some(Metadata.Product(productName, fields)) if (fields.nonEmpty) =>
                  tryAllKeys(desc.desc, Some(productName.originalName), productName.alternativeNames)
                case Some(_)                                                          => desc.desc
                case None                                                             => desc.desc
              }
            )
            .reduce(_ orElse _)

        case Some(keyName) =>
          Config
            .string(keyName)
            .switch(
              allDescs.flatMap { desc =>
                desc.metadata match {
                  case Some(Metadata.Object(name, value)) =>
                    List(name.originalName -> Config.Constant(value.asInstanceOf[T]))

                  case Some(m) =>
                    (m.originalName :: m.alternativeNames).map(_ -> desc.desc)

                  case None => Nil
                }
              }*
            )
      }

    DeriveConfig.from(desc)
  }

  /** Adds the default value from `defaultValues` to the instance of each field in `fieldNames` that has one. */
  def addDefaultValues(
    defaultValues: Map[String, Any],
    fieldNames: List[String],
    descriptors: List[DeriveConfig[Any]]
  ): List[DeriveConfig[_]] =
    descriptors.zip(fieldNames).map { case (desc, fieldName) =>
      defaultValues.get(fieldName) match {
        case Some(any) => DeriveConfig(desc.desc.withDefault(any), desc.metadata)
        case None      => desc
      }
    }

  /** Same as the overload without `g`; `g` is not used. */
  @deprecated("use overloaded method without providing `g`")
  def mergeAllFields[T](
    allDescs: => List[DeriveConfig[_]],
    productName: ProductName,
    fieldNames: => List[FieldName],
    f: List[Any] => T,
    g: T => List[Any]
  ): DeriveConfig[T] =
    mergeAllFields[T](allDescs, productName, fieldNames, f)

  /**
   * Builds the instance for a case class from the instances of its fields: each field is read under its key, with its
   * `@describe` texts, and `f` builds the value. A case class without fields is read as its name or one of its `@name`
   * values.
   */
  def mergeAllFields[T](
    allDescs: => List[DeriveConfig[_]],
    productName: ProductName,
    fieldNames: List[FieldName],
    f: List[Any] => T
  ): DeriveConfig[T] =
    if fieldNames.isEmpty then { // if there are no fields in the product then the value is the name of the product itself
      val tryAllPaths =
        (productName.originalName :: productName.alternativeNames)
          .map(n => zio.Config.constant(n))
          .reduce(_ orElse _)

      DeriveConfig(
        tryAllPaths.map[T](_ => f(Nil)),
        Some(Metadata.Object[T](productName, f(Nil))) // We propogate the info that product was actually an object
      )
    } else {
      val listOfDesc =
        fieldNames.zip(allDescs).map { case (fieldName, desc) =>
          val fieldDesc = tryAllKeys(desc.desc, Some(fieldName.originalName), fieldName.alternativeNames)
          fieldName.descriptions.foldRight(fieldDesc)((doc, desc) => desc ?? doc)
        }

      val descOfList =
        Config.collectAll(listOfDesc.head, listOfDesc.tail*)

      DeriveConfig(descOfList.map(f), Some(Metadata.Product(productName, fieldNames)))
    }

  /**
   * Reads `desc` under each of `alternativeKeys` in turn, or under `originalKey` when `alternativeKeys` is empty. With
   * neither, it returns `desc` unchanged.
   */
  def tryAllKeys[A](
    desc: Config[A],
    originalKey: Option[String],
    alternativeKeys: List[String]
  ): Config[A] =
    alternativeKeys match {
      case Nil  => originalKey.fold(desc)(desc.nested(_))
      case keys => keys.view.map(desc.nested(_)).reduce(_ orElse _)
    }

  /** Casts `a` to `T`, without a check. */
  def castTo[T](a: Any): T =
    a.asInstanceOf[T]

  private def keyModifiersOfImpl[T: Type](using Quotes): Expr[(List[KeyModifier], CaseModifier)] =
    val prefixesExpr     = Macros.anns[T, prefix]("zio.config.derivation.prefix")
    val postfixesExpr    = Macros.anns[T, postfix]("zio.config.derivation.postfix")
    val kebabsExpr       = Macros.anns[T, kebabCase]("zio.config.derivation.kebabCase")
    val kebabsLegacyExpr = Macros.anns[T, kebabCaseLegacy]("zio.config.derivation.kebabCaseLegacy")
    val snakesExpr       = Macros.anns[T, snakeCase]("zio.config.derivation.snakeCase")

    val modifiersExpr: Expr[List[KeyModifier]] = '{
      $prefixesExpr.map(p => KeyModifier.Prefix(p.prefix)) :::
        $postfixesExpr.map(p => KeyModifier.Postfix(p.postfix))
    }

    val caseModifierExpr: Expr[CaseModifier] = '{
      val kebabs       = $kebabsExpr
      val kebabsLegacy = $kebabsLegacyExpr
      val snakes       = $snakesExpr
      if (kebabs.nonEmpty) KeyModifier.KebabCase
      else if (kebabsLegacy.nonEmpty) KeyModifier.KebabCaseLegacy
      else if (snakes.nonEmpty) KeyModifier.SnakeCase
      else KeyModifier.NoneModifier
    }

    '{ ($modifiersExpr, $caseModifierExpr) }
}
