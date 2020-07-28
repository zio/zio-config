package zio.config.shapeless

import java.io.File
import java.net.{ URI, URL }
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID

import shapeless._
import shapeless.labelled._
import zio.config._
import zio.config.derivation.DerivationUtils.constant
import zio.config.derivation.NeedsDerive
import zio.duration.Duration

import scala.concurrent.duration.{ Duration => ScalaDuration }
import scala.reflect.ClassTag

/**
 *
 * `zio-config-shapeless` is an alternative to `zio-config-magnolia` to support scala 2.11 projects.
 * It will be deprecated once we find users have moved on from scala 2.11.
 *
 * DeriveConfigDescriptor.descriptor[Config] gives an automatic ConfigDescriptor for the case class Config recursively
 *
 * DeriveConfigDescriptor.descriptor[X] gives an automatic ConfigDescriptor for the sealed trait X (coproduct)
 *
 * {{{
 *
 *    // Given
 *    final case class Config(username: String, age: Int)
 *
 *    // should work with no additional code
 *    val description = descriptor[Config]
 *
 *    val config = Config.fromSystemEnv(description)
 *
 * }}}
 *
 *
 * Please find more (complex) examples in the examples module in zio-config
 */
object DeriveConfigDescriptor extends DeriveConfigDescriptor {
  def mapClassName(name: String): String = toSnakeCase(name)
  def mapFieldName(name: String): String = name

  val wrapSealedTraitClasses: Boolean = true
  val wrapSealedTraits: Boolean       = false

  /**
   * By default this method is not implicit to allow custom non-recursive derivation
   * */
  override implicit def getDescriptor[T: NeedsDerive: ClassDescriptor]: Descriptor[T] = super.getDescriptor[T]
  implicit def descriptor[T: Descriptor]: ConfigDescriptor[T]                         = Descriptor[T].configDescriptor
}

/**
 * Non-recursive derivation
 *
 * */
object NonRecursiveDerivation extends DeriveConfigDescriptor {
  def mapClassName(name: String): String = zio.config.toSnakeCase(name)
  def mapFieldName(name: String): String = name

  val wrapSealedTraitClasses: Boolean = true
  val wrapSealedTraits: Boolean       = false
}

trait DeriveConfigDescriptor {
  import zio.config.ConfigDescriptor._

  case class ClassDescriptor[T](desc: ConfigDescriptor[T], name: String, isObject: Boolean)

  case class Descriptor[T](configDescriptor: ConfigDescriptor[T]) {
    final def ??(description: String): Descriptor[T] =
      describe(description)

    def default(value: T): Descriptor[T] =
      Descriptor(configDescriptor.default(value))

    def describe(description: String): Descriptor[T] =
      Descriptor(configDescriptor.describe(description))

    def from(that: ConfigSource): Descriptor[T] =
      Descriptor(configDescriptor.from(that))

    def transform[B](f: T => B, g: B => T): Descriptor[B] =
      xmap(f, g)

    def transformEither[B](f: T => Either[String, B], g: B => Either[String, T]): Descriptor[B] =
      xmapEither(f, g)

    def transformEitherLeft[B](f: T => Either[String, B], g: B => T): Descriptor[B] =
      Descriptor(configDescriptor.transformEitherLeft(f, g))

    def transformEitherLeft[E, B](f: T => Either[E, B])(g: B => T)(h: E => String): Descriptor[B] =
      Descriptor(configDescriptor.transformEitherLeft[E, B](f)(g)(h))

    def transformEitherRight[E, B](f: T => B, g: B => Either[String, T]): Descriptor[B] =
      Descriptor(configDescriptor.transformEitherRight(f, g))

    def transformEitherRight[E, B](f: T => B)(g: B => Either[E, T])(h: E => String): Descriptor[B] =
      Descriptor(configDescriptor.transformEitherRight[E, B](f)(g)(h))

    def xmap[B](f: T => B, g: B => T): Descriptor[B] =
      Descriptor(configDescriptor.xmap(f, g))

    def xmapEither[B](f: T => Either[String, B], g: B => Either[String, T]): Descriptor[B] =
      Descriptor(configDescriptor.xmapEither(f, g))

    def xmapEither[E, B](f: T => Either[E, B])(g: B => Either[E, T])(h: E => String): Descriptor[B] =
      Descriptor(configDescriptor.xmapEither[E, B](f)(g)(h))
  }

  object Descriptor {
    def apply[A](implicit ev: Descriptor[A]): Descriptor[A] = ev
  }

  implicit val implicitStringDesc: Descriptor[String]                   = Descriptor(string)
  implicit val implicitBooleanDesc: Descriptor[Boolean]                 = Descriptor(boolean)
  implicit val implicitByteDesc: Descriptor[Byte]                       = Descriptor(byte)
  implicit val implicitShortDesc: Descriptor[Short]                     = Descriptor(short)
  implicit val implicitIntDesc: Descriptor[Int]                         = Descriptor(int)
  implicit val implicitLongDesc: Descriptor[Long]                       = Descriptor(long)
  implicit val implicitBigIntDesc: Descriptor[BigInt]                   = Descriptor(bigInt)
  implicit val implicitFloatDesc: Descriptor[Float]                     = Descriptor(float)
  implicit val implicitDoubleDesc: Descriptor[Double]                   = Descriptor(double)
  implicit val implicitBigDecimalDesc: Descriptor[BigDecimal]           = Descriptor(bigDecimal)
  implicit val implicitUriDesc: Descriptor[URI]                         = Descriptor(uri)
  implicit val implicitUrlDesc: Descriptor[URL]                         = Descriptor(url)
  implicit val implicitScalaDurationDesc: Descriptor[ScalaDuration]     = Descriptor(duration)
  implicit val implicitDurationDesc: Descriptor[Duration]               = Descriptor(zioDuration)
  implicit val implicitUUIDDesc: Descriptor[UUID]                       = Descriptor(uuid)
  implicit val implicitLocalDateDesc: Descriptor[LocalDate]             = Descriptor(localDate)
  implicit val implicitLocalTimeDesc: Descriptor[LocalTime]             = Descriptor(localTime)
  implicit val implicitLocalDateTimeDesc: Descriptor[LocalDateTime]     = Descriptor(localDateTime)
  implicit val implicitInstantDesc: Descriptor[Instant]                 = Descriptor(instant)
  implicit val implicitFileDesc: Descriptor[File]                       = Descriptor(file)
  implicit val implicitJavaFilePathDesc: Descriptor[java.nio.file.Path] = Descriptor(javaFilePath)

  implicit def implicitListDesc[A: Descriptor]: Descriptor[List[A]] =
    Descriptor(listDesc(implicitly[Descriptor[A]].configDescriptor))

  implicit def implicitSetDesc[A: Descriptor]: Descriptor[Set[A]] =
    Descriptor(setDesc(implicitly[Descriptor[A]].configDescriptor))

  implicit def implicitMapDesc[A: Descriptor]: Descriptor[Map[String, A]] =
    Descriptor(mapDesc(implicitly[Descriptor[A]].configDescriptor))

  implicit def implicitEitherDesc[A: Descriptor, B: Descriptor]: Descriptor[Either[A, B]] =
    Descriptor(eitherDesc(implicitly[Descriptor[A]].configDescriptor, implicitly[Descriptor[B]].configDescriptor))

  implicit def implicitOptionDesc[A: Descriptor]: Descriptor[Option[A]] =
    Descriptor(optionDesc(implicitly[Descriptor[A]].configDescriptor))

  protected def listDesc[A](desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]]       = list(desc)
  protected def setDesc[A](desc: ConfigDescriptor[A]): ConfigDescriptor[Set[A]]         = set(desc)
  protected def mapDesc[A](desc: ConfigDescriptor[A]): ConfigDescriptor[Map[String, A]] = map(desc)
  protected def optionDesc[A](desc: ConfigDescriptor[A]): ConfigDescriptor[Option[A]]   = desc.optional

  protected def eitherDesc[A, B](
    left: ConfigDescriptor[A],
    right: ConfigDescriptor[B]
  ): ConfigDescriptor[Either[A, B]] =
    left.orElseEither(right)

  trait CollectClassFields[L <: HList, Names <: HList, Descs <: HList, Defaults <: HList] {
    def apply(names: Names, descs: Descs, defaults: Defaults): ConfigDescriptor[L]
  }

  object CollectClassFields {
    private def makeDescriptor[V](
      desc: Descriptor[V],
      manualName: Option[name],
      fieldName: String,
      defaultValue: Option[V],
      description: Option[describe]
    ) = {
      val name         = manualName.map(_.name).getOrElse(mapFieldName(fieldName))
      val withDefaults = defaultValue.fold(desc.configDescriptor)(desc.configDescriptor.default(_))
      val described    = description.map(_.describe).toSeq.foldLeft(withDefaults)(_ ?? _)
      nested(name)(described)
    }

    implicit def caseHNil[K <: Symbol, V, N, Ds, Df](
      implicit key: Witness.Aux[K],
      desc: Descriptor[V],
      evN: N <:< Option[name],
      evDs: Ds <:< Option[describe],
      evDf: Df <:< Option[V]
    ): CollectClassFields[FieldType[K, V] :: HNil, N :: HNil, Ds :: HNil, Df :: HNil] =
      (names, descs, defaults) => {
        val fieldDesc = makeDescriptor(
          desc = desc,
          manualName = evN(names.head),
          fieldName = key.value.name,
          defaultValue = evDf(defaults.head),
          description = evDs(descs.head)
        )
        fieldDesc.xmap(field[K](_) :: HNil, _.head)
      }

    implicit def caseHCons[K <: Symbol, V, T <: HList, N, Ds, Df, TN <: HList, TDs <: HList, TDf <: HList](
      implicit key: Witness.Aux[K],
      desc: Descriptor[V],
      evN: N <:< Option[name],
      evDs: Ds <:< Option[describe],
      evDf: Df <:< Option[V],
      next: CollectClassFields[T, TN, TDs, TDf]
    ): CollectClassFields[FieldType[K, V] :: T, N :: TN, Ds :: TDs, Df :: TDf] =
      (names, descs, defaults) => {
        val fieldDesc = makeDescriptor(
          desc = desc,
          manualName = evN(names.head),
          fieldName = key.value.name,
          defaultValue = evDf(defaults.head),
          description = evDs(descs.head)
        )
        (fieldDesc |@| next(names.tail, descs.tail, defaults.tail))(
          (v, t) => field[K](v) :: t,
          l => Some((l.head, l.tail))
        )
      }
  }

  trait OptAnnotation[A, T] {
    def apply(): Option[A]
  }

  trait LowPriorityOptAnnotation {
    implicit def caseNone[A, T]: OptAnnotation[A, T] = () => None
  }

  object OptAnnotation extends LowPriorityOptAnnotation {
    implicit def caseSome[A, T](implicit an: Annotation[A, T]): OptAnnotation[A, T] = () => Some(an())
  }

  implicit def objectDescriptor[T](
    implicit gen: LabelledGeneric.Aux[T, HNil],
    typeName: TypeName[T],
    optName: OptAnnotation[name, T],
    optDesc: OptAnnotation[describe, T]
  ): ClassDescriptor[T] = {
    val ccName = optName().map(_.name).getOrElse(mapClassName(typeName()))
    val desc   = constant[T](ccName, gen.from(HNil))
    ClassDescriptor(optDesc().map(_.describe).fold(desc)(desc ?? _), ccName, isObject = true)
  }

  implicit def classDescriptor[T, Repr <: HList, Names <: HList, Descs <: HList, Defaults <: HList](
    implicit gen: LabelledGeneric.Aux[T, Repr],
    typeName: TypeName[T],
    optName: OptAnnotation[name, T],
    optDesc: OptAnnotation[describe, T],
    names: Annotations.Aux[name, T, Names],
    descs: Annotations.Aux[describe, T, Descs],
    defaults: Default.Aux[T, Defaults],
    collectFields: CollectClassFields[Repr, Names, Descs, Defaults]
  ): ClassDescriptor[T] = {
    val ccName = optName().map(_.name).getOrElse(mapClassName(typeName()))

    val desc = collectFields(names(), descs(), defaults()).xmap(gen.from, gen.to)

    ClassDescriptor(optDesc().map(_.describe).fold(desc)(desc ?? _), ccName, isObject = false)
  }

  trait SumCase[C] {
    def desc: ClassDescriptor[C]
    def cast(a: Any): Option[C]
  }

  trait CollectSum[Repr <: Coproduct] {
    def apply(): List[SumCase[_]]
  }

  object CollectSum {
    implicit val caseCNil: CollectSum[CNil] = () => Nil
    implicit def caseCCons[H, T <: Coproduct](
      implicit desc0: ClassDescriptor[H],
      ct: ClassTag[H],
      next: CollectSum[T]
    ): CollectSum[H :+: T] =
      () =>
        new SumCase[H] {
          val desc: ClassDescriptor[H] = desc0
          def cast(a: Any): Option[H] = a match {
            case h: H => Some(h)
            case _    => None
          }
        } :: next()
  }

  implicit def sumDescriptor[T, Repr <: Coproduct](
    implicit gen: Generic.Aux[T, Repr],
    cs: CollectSum[Repr],
    typeName: TypeName[T],
    optName: OptAnnotation[name, T]
  ): ClassDescriptor[T] = {
    val _ = gen

    def mapCase[C](sc: SumCase[C]) = {
      val desc =
        if (sc.desc.isObject || !wrapSealedTraitClasses) sc.desc.desc
        else nested(sc.desc.name)(sc.desc.desc)

      wrapSealedTrait(optName().map(_.name).getOrElse(mapClassName(typeName())), desc).xmapEither[T](
        st => Right(st.asInstanceOf[T]),
        t => sc.cast(t).map(Right(_)).getOrElse(Left(s"Expected ${sc.desc.name}, but got ${t.getClass.getName}"))
      )

    }

    val desc = cs().map(mapCase(_)).reduce(_.orElse(_))
    ClassDescriptor(desc, typeName(), isObject = false)
  }

  def mapClassName(name: String): String

  def mapFieldName(name: String): String

  def wrapSealedTraitClasses: Boolean

  def wrapSealedTraits: Boolean

  final def wrapSealedTrait[T](
    label: String,
    desc: ConfigDescriptor[T]
  ): ConfigDescriptor[T] =
    if (wrapSealedTraits) nested(label)(desc)
    else desc

  def getDescriptor[T: NeedsDerive: ClassDescriptor]: Descriptor[T] = Descriptor(implicitly[ClassDescriptor[T]].desc)
  def apply[T: NeedsDerive: ClassDescriptor]: Descriptor[T]         = getDescriptor[T]
}
