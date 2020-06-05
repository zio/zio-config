package zio.config.shapeless

import java.io.File
import java.net.{ URI, URL }
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID

import shapeless._
import shapeless.labelled._
import zio.config._
import zio.config.derivation.DerivationUtils.constant
import zio.config.derivation.{ DerivationUtils, NeedsDerive }
import zio.duration.Duration

import scala.concurrent.duration.{ Duration => ScalaDuration }
import scala.reflect.ClassTag

/**
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
  val wrapSealedTraits: Boolean       = true

  /**
   * By default this method is not implicit to allow custom non-recursive derivation
   * */
  override implicit def descriptor[T: NeedsDerive: ClassDescriptor]: Descriptor[T] = super.descriptor[T]
}

/**
 * Non-recursive derivation
 *
 * */
object NonRecursiveDerivation extends DeriveConfigDescriptor {
  def mapClassName(name: String): String = toSnakeCase(name)
  def mapFieldName(name: String): String = name

  val wrapSealedTraitClasses: Boolean = true
  val wrapSealedTraits: Boolean       = true
}

trait DeriveConfigDescriptor {
  import zio.config.ConfigDescriptor._

  case class ClassDescriptor[T](desc: ConfigDescriptor[T], name: String, isObject: Boolean)

  case class Descriptor[T](desc: ConfigDescriptor[T])

  object Descriptor {
    implicit def toConfigDescriptor[T](ev: Descriptor[T]): ConfigDescriptor[T] = ev.desc
  }

  protected def stringDesc: ConfigDescriptor[String]                   = string
  protected def booleanDesc: ConfigDescriptor[Boolean]                 = boolean
  protected def byteDesc: ConfigDescriptor[Byte]                       = byte
  protected def shortDesc: ConfigDescriptor[Short]                     = short
  protected def intDesc: ConfigDescriptor[Int]                         = int
  protected def longDesc: ConfigDescriptor[Long]                       = long
  protected def bigIntDesc: ConfigDescriptor[BigInt]                   = bigInt
  protected def floatDesc: ConfigDescriptor[Float]                     = float
  protected def doubleDesc: ConfigDescriptor[Double]                   = double
  protected def bigDecimalDesc: ConfigDescriptor[BigDecimal]           = bigDecimal
  protected def uriDesc: ConfigDescriptor[URI]                         = uri
  protected def urlDesc: ConfigDescriptor[URL]                         = url
  protected def scalaDurationDesc: ConfigDescriptor[ScalaDuration]     = duration
  protected def durationDesc: ConfigDescriptor[Duration]               = zioDuration
  protected def uuidDesc: ConfigDescriptor[UUID]                       = uuid
  protected def localDateDesc: ConfigDescriptor[LocalDate]             = localDate
  protected def localTimeDesc: ConfigDescriptor[LocalTime]             = localTime
  protected def localDateTimeDesc: ConfigDescriptor[LocalDateTime]     = localDateTime
  protected def instantDesc: ConfigDescriptor[Instant]                 = instant
  protected def fileDesc: ConfigDescriptor[File]                       = file
  protected def javaFilePathDesc: ConfigDescriptor[java.nio.file.Path] = javaFilePath

  implicit val implicitStringDesc: Descriptor[String]                   = Descriptor(stringDesc)
  implicit val implicitBooleanDesc: Descriptor[Boolean]                 = Descriptor(booleanDesc)
  implicit val implicitByteDesc: Descriptor[Byte]                       = Descriptor(byteDesc)
  implicit val implicitShortDesc: Descriptor[Short]                     = Descriptor(shortDesc)
  implicit val implicitIntDesc: Descriptor[Int]                         = Descriptor(intDesc)
  implicit val implicitLongDesc: Descriptor[Long]                       = Descriptor(longDesc)
  implicit val implicitBigIntDesc: Descriptor[BigInt]                   = Descriptor(bigIntDesc)
  implicit val implicitFloatDesc: Descriptor[Float]                     = Descriptor(floatDesc)
  implicit val implicitDoubleDesc: Descriptor[Double]                   = Descriptor(doubleDesc)
  implicit val implicitBigDecimalDesc: Descriptor[BigDecimal]           = Descriptor(bigDecimalDesc)
  implicit val implicitUriDesc: Descriptor[URI]                         = Descriptor(uriDesc)
  implicit val implicitUrlDesc: Descriptor[URL]                         = Descriptor(urlDesc)
  implicit val implicitScalaDurationDesc: Descriptor[ScalaDuration]     = Descriptor(scalaDurationDesc)
  implicit val implicitDurationDesc: Descriptor[Duration]               = Descriptor(durationDesc)
  implicit val implicitUUIDDesc: Descriptor[UUID]                       = Descriptor(uuidDesc)
  implicit val implicitLocalDateDesc: Descriptor[LocalDate]             = Descriptor(localDateDesc)
  implicit val implicitLocalTimeDesc: Descriptor[LocalTime]             = Descriptor(localTimeDesc)
  implicit val implicitLocalDateTimeDesc: Descriptor[LocalDateTime]     = Descriptor(localDateTimeDesc)
  implicit val implicitInstantDesc: Descriptor[Instant]                 = Descriptor(instantDesc)
  implicit val implicitFileDesc: Descriptor[File]                       = Descriptor(fileDesc)
  implicit val implicitJavaFilePathDesc: Descriptor[java.nio.file.Path] = Descriptor(javaFilePathDesc)

  implicit def implicitListDesc[A: Descriptor]: Descriptor[List[A]] =
    Descriptor(listDesc(implicitly[Descriptor[A]].desc))

  implicit def implicitSetDesc[A: Descriptor]: Descriptor[Set[A]] =
    Descriptor(setDesc(implicitly[Descriptor[A]].desc))

  implicit def implicitMapDesc[A: Descriptor]: Descriptor[Map[String, A]] =
    Descriptor(mapDesc(implicitly[Descriptor[A]].desc))

  implicit def implicitEitherDesc[A: Descriptor, B: Descriptor]: Descriptor[Either[A, B]] =
    Descriptor(eitherDesc(implicitly[Descriptor[A]].desc, implicitly[Descriptor[B]].desc))

  implicit def implicitOptionDesc[A: Descriptor]: Descriptor[Option[A]] =
    Descriptor(optionDesc(implicitly[Descriptor[A]].desc))

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
      val withDefaults = defaultValue.fold(desc.desc)(desc.default(_))
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

  def toSnakeCase(name: String): String = DerivationUtils.toSnakeCase(name)

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

  def descriptor[T: NeedsDerive: ClassDescriptor]: Descriptor[T] = Descriptor(implicitly[ClassDescriptor[T]].desc)
  def apply[T: NeedsDerive: ClassDescriptor]: Descriptor[T]      = descriptor[T]
}
