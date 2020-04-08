package zio.config.magnolia

import java.net.URI

import magnolia._
import zio.config.ConfigDescriptor
import zio.config.ConfigDescriptor._

import scala.language.experimental.macros
import scala.util.{ Failure, Success }

/**
 * DeriveConfigDescriptor[Config] gives an automatic ConfigDescriptor for the case class Config
 * given each field in the case class has an instance of DeriveConfigDescriptor
 *
 * DeriveConfigDescriptor[X] gives an automatic ConfigDescriptor for the sealed trait X (coproduct)
 * given each term in the coproduct has an instance of DeriveConfigDescriptor
 *
 * {{{
 *
 *    // Given
 *    final case class Config(username: String, age: Int)
 *
 *    // should work with no additional code
 *    val description = DeriveConfigDescriptor[Config]
 *
 *    val config = Config.fromSystemEnv(description)
 *
 * Please find more (complex) examples in the examples module in zio-config
 *
 * }}}
 */
trait DeriveConfigDescriptor[A] { self =>
  def getDescription(path: Option[String], parentClass: Option[String]): ConfigDescriptor[String, String, A]

  def xmap[B](f: A => B)(g: B => A): DeriveConfigDescriptor[B] =
    xmapEither(a => Right(f(a)))(b => Right(g(b)))

  def xmapEither[B](f: A => Either[String, B])(g: B => Either[String, A]): DeriveConfigDescriptor[B] =
    new DeriveConfigDescriptor[B] {
      override def getDescription(
        path: Option[String],
        parentClass: Option[String]
      ): ConfigDescriptor[String, String, B] =
        self.getDescription(path, parentClass).xmapEither(a => f(a), (b: B) => g(b))
    }
}

trait LowPriorityDeriveConfigDescriptor {
  implicit def opt[A](implicit ev: DeriveConfigDescriptor[A]): DeriveConfigDescriptor[Option[A]] =
    (a, b) => ev.getDescription(a, b).optional
}

object DeriveConfigDescriptor extends LowPriorityDeriveConfigDescriptor {

  /**
   * DeriveConfigDescriptor[Config] gives an automatic ConfigDescriptor for the case class Config
   * given each field in the case class has an instance of DeriveConfigDescriptor
   *
   * DeriveConfigDescriptor[X] gives an automatic ConfigDescriptor for the sealed trait X (coproduct)
   * given each term in the coproduct has an instance of DeriveConfigDescriptor
   *
   * {{{
   *
   *    // Given
   *    final case class Config(username: String, age: Int)
   *
   *    // should work with no additional code
   *    val description = DeriveConfigDescriptor[Config]
   *
   *    val config = Config.fromSystemEnv(description)
   *
   * }}}
   *
   * Please find more (complex) examples in the examples module in zio-config
   */
  def apply[T](implicit ev: DeriveConfigDescriptor[T]): ConfigDescriptor[String, String, T] =
    ev.getDescription(None, None)

  /**
   * DeriveConfigDescriptor.descriptor[Config] gives an automatic ConfigDescriptor for the case class Config
   * given each field in the case class has an instance of DeriveConfigDescriptor
   *
   * DeriveConfigDescriptor[X] gives an automatic ConfigDescriptor for the sealed trait X (coproduct)
   * given each term in the coproduct has an instance of DeriveConfigDescriptor
   *
   * {{{
   *
   *    // Given
   *    final case class Config(username: String, age: Int)
   *
   *    // should work with no additional code
   *    val description = DeriveConfigDescriptor[Config]
   *
   *    val config = Config.fromSystemEnv(description)
   *
   * }}}
   *
   * Please find more (complex) examples in the examples module in zio-config
   */
  def descriptor[T](implicit ev: DeriveConfigDescriptor[T]): ConfigDescriptor[String, String, T] =
    ev.getDescription(None, None)

  def instance[T](f: String => ConfigDescriptor[String, String, T]): DeriveConfigDescriptor[T] =
    new DeriveConfigDescriptor[T] {
      override def getDescription(
        path: Option[String],
        parentClass: Option[String]
      ): ConfigDescriptor[String, String, T] =
        path.fold(
          string.xmapEither[T](
            _ => Left("unable to fetch the primitive without a path"),
            (_: T) => Left("unable to write the primitive back to a config source without a path")
          )
        )(f)
    }

  implicit val stringDesc: DeriveConfigDescriptor[String]         = instance(string)
  implicit val booleanDesc: DeriveConfigDescriptor[Boolean]       = instance(boolean)
  implicit val byteDesc: DeriveConfigDescriptor[Byte]             = instance(byte)
  implicit val shortDesc: DeriveConfigDescriptor[Short]           = instance(short)
  implicit val intDesc: DeriveConfigDescriptor[Int]               = instance(int)
  implicit val longDesc: DeriveConfigDescriptor[Long]             = instance(long)
  implicit val bigIntDesc: DeriveConfigDescriptor[BigInt]         = instance(bigInt)
  implicit val floatDesc: DeriveConfigDescriptor[Float]           = instance(float)
  implicit val doubleDesc: DeriveConfigDescriptor[Double]         = instance(double)
  implicit val bigDecimalDesc: DeriveConfigDescriptor[BigDecimal] = instance(bigDecimal)
  implicit val uriDesc: DeriveConfigDescriptor[URI]               = instance(uri)

  /**
   * Leaks out path in simple cases, i.e.
   * `legacyList(string(path))` == `list(path)(string)` == `nested(path)(list(string))`
   *
   * `nested("a")(legacyList(string("b")))` describes configuration `{a: { b: ["s1", "s2"] }}`
   *
   * Allows scalar value instead of list
   * */
  def legacyList[K, V, A](desc: ConfigDescriptor[K, V, A]): ConfigDescriptor[K, V, List[A]] = {
    def extractPath(cfg: ConfigDescriptor[K, V, A]): Option[(K, ConfigDescriptor[K, V, A])] = cfg match {
      case Describe(config, message) =>
        extractPath(config).map { case (path, inner) => (path, Describe(inner, message)) }
      case Nested(path, config) => Some((path, config))
      case _                    => None
    }

    extractPath(desc) match {
      case Some((path, inner)) => list(path)(inner)
      case None                => list(desc)
    }
  }

  implicit def optNonEmptyList[A](implicit ev: DeriveConfigDescriptor[A]): DeriveConfigDescriptor[Option[::[A]]] =
    (a, b) =>
      legacyList(ev.getDescription(a, b)).optional.xmap(
        {
          case None | Some(Nil)   => None
          case Some(head :: tail) => Some(::(head, tail))
        },
        x => x
      )

  implicit def listt[A](implicit ev: DeriveConfigDescriptor[A]): DeriveConfigDescriptor[List[A]] =
    (a, b) => legacyList(ev.getDescription(a, b))

  implicit def nonEmptyList[A](implicit ev: DeriveConfigDescriptor[A]): DeriveConfigDescriptor[::[A]] =
    (a, b) =>
      legacyList(ev.getDescription(a, b)).xmapEither(
        list =>
          list.headOption match {
            case Some(value) => Right(::(value, list.tail))
            case None =>
              Left(
                "The list is empty. Either provide a non empty list, and if not mark it as optional and choose to avoid it in the config"
              )
          },
        ((nonEmpty: ::[A]) => Right(nonEmpty.toList))
      )

  // This is equivalent to saying string("PATH").orElseEither(int("PATH")). During automatic derivations, we are unaware of alternate paths.
  implicit def eith[A: DeriveConfigDescriptor, B: DeriveConfigDescriptor]: DeriveConfigDescriptor[Either[A, B]] =
    new DeriveConfigDescriptor[Either[A, B]] {
      override def getDescription(
        path: Option[String],
        parentClass: Option[String]
      ): ConfigDescriptor[String, String, Either[A, B]] =
        implicitly[DeriveConfigDescriptor[A]]
          .getDescription(path, parentClass)
          .orElseEither(implicitly[DeriveConfigDescriptor[B]].getDescription(path, parentClass))
    }

  type Typeclass[T] = DeriveConfigDescriptor[T]

  def combine[T](caseClass: CaseClass[DeriveConfigDescriptor, T]): DeriveConfigDescriptor[T] =
    new DeriveConfigDescriptor[T] {
      def getDescription(path: Option[String], parentClass: Option[String]): ConfigDescriptor[String, String, T] = {
        // A complex nested inductive resolution for parent paths (separately handled for case objects and classes) to get the ergonomics right in the hocon source.
        val finalDesc =
          if (caseClass.isObject) {
            val config = parentClass match {
              case Some(parentClass) => string(parentClass.toLowerCase())

              case None =>
                string.xmapEither[String](
                  _ =>
                    Left(
                      s"Cannot create the case-object ${caseClass.typeName.short} since it is not part of a coproduct (ie. extends sealed trait)"
                    ),
                  (_: String) =>
                    Left(
                      s"Cannot write the case-object ${caseClass.typeName.short} since it is not part of a coproduct (i.e, extends sealed trait)"
                    )
                )
            }
            config.xmapEither(
              str =>
                if (str == caseClass.typeName.short.toLowerCase()) Right(caseClass.rawConstruct(Seq.empty))
                else
                  Left("Not enough details in the config source to form the config using automatic derviation."),
              (value: Any) => Right(value.toString.toLowerCase)
            )
          } else {
            val listOfConfigs: List[ConfigDescriptor[String, String, Any]] =
              caseClass.parameters.toList.map { h =>
                val rawDesc =
                  h.typeclass.getDescription(Some(h.label), None)

                val descriptions =
                  h.annotations
                    .filter(_.isInstanceOf[describe])
                    .map(_.asInstanceOf[describe].describe)

                val withDefault =
                  h.default
                    .map(r => rawDesc.default(r))
                    .getOrElse(rawDesc)

                val withDocs =
                  updateConfigWithDocuments(descriptions, withDefault)

                val config = withDocs.xmap((r: h.PType) => r: Any, (r: Any) => r.asInstanceOf[h.PType])

                parentClass.fold(config)(_ => nested(caseClass.typeName.short.toLowerCase())(config))

              }

            collectAll(::(listOfConfigs.head, listOfConfigs.tail))
              .xmap[T](
                cons => caseClass.rawConstruct(cons),
                v => {
                  val r = caseClass.parameters.map(_.dereference(v): Any).toList
                  ::(r.head, r.tail)
                }
              )
          }

        val annotations = caseClass.annotations
          .filter(_.isInstanceOf[zio.config.magnolia.describe])
          .map(_.asInstanceOf[describe].describe)

        val withParent =
          if (caseClass.isObject) finalDesc
          else
            parentClass.fold(finalDesc)(parentClass => nested(parentClass.toLowerCase())(finalDesc))

        updateConfigWithDocuments(
          annotations,
          path.fold(withParent)(nested(_)(withParent))
        )
      }
    }

  def dispatch[T](sealedTrait: SealedTrait[DeriveConfigDescriptor, T]): DeriveConfigDescriptor[T] =
    new DeriveConfigDescriptor[T] {
      def getDescription(paths: Option[String], parentClass: Option[String]): ConfigDescriptor[String, String, T] = {
        val list         = sealedTrait.subtypes.toList
        val head :: tail = ::(list.head, list.tail)

        tail.foldRight[ConfigDescriptor[String, String, T]](
          head.typeclass
            .getDescription(paths, Some(sealedTrait.typeName.short.toLowerCase()))
            .xmapEither(
              (t: head.SType) => Right(t: T), { a: T =>
                scala.util.Try(head.cast(a)) match {
                  case Success(value) => Right(value)
                  case Failure(value) => Left(s"Failure when trying to write: ${value.getMessage}")
                }
              }
            )
        )(
          (e: Subtype[Typeclass, T], b: ConfigDescriptor[String, String, T]) =>
            b.orElse(
              e.typeclass
                .getDescription(paths, Some(sealedTrait.typeName.short.toLowerCase()))
                .xmapEither(
                  (t: e.SType) => Right(t: T),
                  (a: T) =>
                    scala.util.Try(e.cast(a)) match {
                      case Success(value) => Right(value)
                      case Failure(value) => Left(s"Failure when trying to write: ${value.getMessage}")
                    }
                )
            )
        )

      }
    }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

  private[config] def updateConfigWithDocuments[K, V, A](
    documents: Seq[String],
    config: ConfigDescriptor[K, V, A]
  ): ConfigDescriptor[K, V, A] =
    documents.foldLeft(config)((cf, doc) => cf ?? doc)
}
