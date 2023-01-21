// package zio.config.magnolia

// import magnolia._
// import zio.config._
// import zio.config.derivation.DerivationUtils._
// import zio.{Duration, NonEmptyChunk}

// import java.io.File
// import java.net.{URI, URL}
// import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
// import java.util.UUID
// import scala.concurrent.duration.{Duration => ScalaDuration}

// import ConfigDescriptorAdt._
// import zio.Config

// case class Descriptor_[T](desc: Config[T], isObject: Boolean = false) {

//   final def ??(description: String): Descriptor_[T] =
//     describe(description)

//   def describe(description: String): Descriptor_[T] =
//     Descriptor_(desc.??(description))

//   def map[B](f: T => B): Descriptor_[B] =
//     Descriptor_(desc.map(f))

//   def mapOrFail[B](f: T => Either[Config.Error, B]): Descriptor_[B] =
//     Descriptor_(desc.mapOrFail(f))

// }

// object Descriptor_ {
//   // The default behaviour of zio-config is to discard the name of a sealed trait
//   def apply[A](implicit ev: Descriptor_[A]): Descriptor_[A] =
//     ev

//   import Config._

//   implicit val implicitStringDesc: Descriptor_[String]               = Descriptor_(string)
//   implicit val implicitBooleanDesc: Descriptor_[Boolean]             = Descriptor_(boolean)
//   implicit val implicitIntDesc: Descriptor_[Int]                     = Descriptor_(int)
//   implicit val implicitBigIntDesc: Descriptor_[BigInt]               = Descriptor_(bigInt)
//   implicit val implicitFloatDesc: Descriptor_[Float]                 = Descriptor_(float)
//   implicit val implicitDoubleDesc: Descriptor_[Double]               = Descriptor_(double)
//   implicit val implicitBigDecimalDesc: Descriptor_[BigDecimal]       = Descriptor_(bigDecimal)
//   implicit val implicitUriDesc: Descriptor_[URI]                     = Descriptor_(uri)
//   implicit val implicitLocalDateDesc: Descriptor_[LocalDate]         = Descriptor_(localDate)
//   implicit val implicitLocalTimeDesc: Descriptor_[LocalTime]         = Descriptor_(localTime)
//   implicit val implicitLocalDateTimeDesc: Descriptor_[LocalDateTime] = Descriptor_(localDateTime)

//   implicit def implicitListDesc[A: Descriptor_]: Descriptor_[List[A]] =
//     Descriptor_(Config.listOf(implicitly[Descriptor_[A]].desc))

//   implicit def implicitSetDesc[A: Descriptor_]: Descriptor_[Set[A]] =
//     Descriptor_(Config.setOf(implicitly[Descriptor_[A]].desc))

//   implicit def implicitMapDesc[K, A: Descriptor_]: Descriptor_[Map[String, A]] =
//     Descriptor_(Config.table(implicitly[Descriptor_[A]].desc))

//   type Typeclass[T] = Descriptor_[T]

//   final def wrapSealedTrait[T](
//     labels: Seq[String],
//     desc: Config[T]
//   ): Config[T] = {
//     val f = (name: String) => desc.nested(name)
//     labels.tail.foldLeft(f(labels.head)) { case (acc, n) =>
//       acc orElse f(n)
//     }
//   }

//   final def prepareClassName(annotations: Seq[Any], name: String): String       =
//     annotations.collectFirst { case d: name => d.name }.getOrElse(name)

//   final def prepareClassNames(annotations: Seq[Any], name: String): Seq[String] =
//     annotations.collectFirst { case d: names => d.names }.getOrElse(List[String]()) ++
//       List(annotations.collectFirst { case d: name => d.name }.getOrElse(name))

//   final def prepareFieldName(annotations: Seq[Any], name: String): String       =
//     annotations.collectFirst { case d: name => d.name }.getOrElse(name)

//   final def prepareFieldNames(annotations: Seq[Any], name: String): Seq[String] =
//     annotations.collectFirst { case d: names => d.names }.getOrElse(List[String]()) ++
//       List(annotations.collectFirst { case d: name => d.name }.getOrElse(name))

//   final def combine[T](caseClass: CaseClass[Descriptor_, T]): Descriptor_[T] = {
//     val descriptions = caseClass.annotations.collect { case d: describe => d.describe }
//     val ccNames      = prepareClassNames(caseClass.annotations, caseClass.typeName.short)

//     val res =
//       caseClass.parameters.toList match {
//         case Nil          =>
//           val f = (name: String) =>
//             constantString(name).transform[T](
//               _ => caseClass.construct(_ => ???),
//               _ => name
//             )
//           ccNames.tail.foldLeft(f(ccNames.head)) { case (acc, n) =>
//             acc orElse f(n)
//           }
//         case head :: tail =>
//           def nest(name: String)(unwrapped: Config[Any])            =
//             if (caseClass.isValueClass) unwrapped
//             else unwrapped.nested(name)
//           def makeNestedParam(name: String, unwrapped: Config[Any]) =
//             nest(name)(unwrapped)

//           def makeDescriptor(param: Param[Descriptor_, T]): Config[Any] = {
//             val descriptions =
//               param.annotations
//                 .filter(_.isInstanceOf[describe])
//                 .map(_.asInstanceOf[describe].describe)

//             val paramNames = prepareFieldNames(param.annotations, param.label)

//             val raw         = param.typeclass.desc
//             val withNesting = paramNames.tail.foldLeft(makeNestedParam(paramNames.head, raw)) { case (acc, name) =>
//               acc orElse makeNestedParam(name, raw)
//             }
//             val described   = descriptions.foldLeft(withNesting)(_ ?? _)
//             param.default.fold(described)(described.withDefault(_))
//           }

//           collectAll(
//             ConfigDescriptorAdt.lazyDesc(makeDescriptor(head)),
//             tail.map(a => ConfigDescriptorAdt.lazyDesc(makeDescriptor(a))): _*
//           ).transform[T](
//             l => caseClass.rawConstruct(l),
//             t => caseClass.parameters.map(_.dereference(t)).toList
//           )
//       }

//     Descriptor(descriptions.foldLeft(res)(_ ?? _), caseClass.isObject || caseClass.parameters.isEmpty)
//   }

//   final def dispatch[T](sealedTrait: SealedTrait[Descriptor_, T]): Descriptor_[T] = {
//     val nameToLabel =
//       sealedTrait.subtypes
//         .map(tc => prepareClassName(tc.annotations, tc.typeName.short) -> tc.typeName.full)
//         .groupBy(_._1)
//         .toSeq
//         .flatMap {
//           case (label, Seq((_, fullName))) => (fullName -> label) :: Nil
//           case (label, seq)                =>
//             seq.zipWithIndex.map { case ((_, fullName), idx) => fullName -> s"${label}_$idx" }
//         }
//         .toMap

//     val desc =
//       sealedTrait.subtypes.map { subtype =>
//         val typeclass: Descriptor[subtype.SType] = subtype.typeclass

//         val subClassName =
//           nameToLabel(subtype.typeName.full)

//         val subClassNames =
//           prepareClassNames(subtype.annotations, subClassName)

//         val desc = {
//           val keyType =
//             if (typeclass.isObject)
//               ConfigDescriptorAdt.KeyType.CaseObject
//             else
//               ConfigDescriptorAdt.KeyType.SubClass

//           val f = (name: String) =>
//             nested(name)(
//               typeclass.desc,
//               Some(keyType)
//             )

//           if (subClassNames.length > 1)
//             subClassNames.tail.foldLeft(f(subClassNames.head)) { case (acc, n) =>
//               acc orElse f(n)
//             }
//           else
//             nested(subClassName)(
//               typeclass.desc,
//               Some(keyType)
//             )
//         }

//         wrapSealedTrait(prepareClassNames(sealedTrait.annotations, sealedTrait.typeName.short), desc)
//           .transformOrFail[T](
//             st => Right(st),
//             t =>
//               subtype.cast
//                 .andThen(Right(_))
//                 .applyOrElse(t, (_: T) => Left(s"Expected ${subtype.typeName.full}, but got ${t.getClass.getName}"))
//           )
//       }.reduce(_.orElse(_))

//     Descriptor(desc)
//   }

//   implicit def getDescriptor_[T]: Descriptor_[T] = macro Magnolia.gen[T]

//   def descriptor_[T](implicit config: Descriptor_[T]): ConfigDescriptor_[T] =
//     descriptorWithClassNames[T]
//       .removeKey(
//         KeyType.SealedTrait
//       )
//       .removeKey(
//         KeyType.CaseObject
//       )

// }
