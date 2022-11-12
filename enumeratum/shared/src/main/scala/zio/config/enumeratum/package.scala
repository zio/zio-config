package zio.config

import _root_.enumeratum._
import _root_.enumeratum.values._

import scala.reflect.ClassTag

import ConfigDescriptor._

package object enumeratum {
  def nonEmptyString: ConfigDescriptor[String] =
    string.transformOrFailLeft(str => if (str.isEmpty) Left("Empty string") else Right(str))(identity)

  def `enum`[A <: EnumEntry](`enum`: Enum[A])(implicit ct: ClassTag[A]): ConfigDescriptor[A] =
    nonEmptyString.transformOrFailLeft(s => `enum`.withNameEither(s).swap.map(_.getMessage()).swap)(_.entryName)

  def intEnum[A <: IntEnumEntry](`enum`: IntEnum[A])(implicit
    ct: ClassTag[A]
  ): ConfigDescriptor[A] =
    nonEmptyString
      .transformOrFailLeft(v => `enum`.withValueEither(v.toInt).swap.map(_.getMessage()).swap)(_.value.toString)

  def longEnum[A <: LongEnumEntry](`enum`: LongEnum[A])(implicit
    ct: ClassTag[A]
  ): ConfigDescriptor[A] =
    nonEmptyString
      .transformOrFailLeft[A](v => `enum`.withValueEither(v.toLong).swap.map(_.getMessage()).swap)(_.value.toString)

  def shortEnum[A <: ShortEnumEntry](`enum`: ShortEnum[A])(implicit
    ct: ClassTag[A]
  ): ConfigDescriptor[A] =
    nonEmptyString
      .transformOrFailLeft[A](v => `enum`.withValueEither(v.toShort).swap.map(_.getMessage()).swap)(_.value.toString)

  def stringEnum[A <: StringEnumEntry](`enum`: StringEnum[A])(implicit
    ct: ClassTag[A]
  ): ConfigDescriptor[A] =
    string
      .transformOrFailLeft[A](v => `enum`.withValueEither(v).swap.map(_.getMessage()).swap)(_.value.toString)

  def byteEnum[A <: ByteEnumEntry](`enum`: ByteEnum[A])(implicit
    ct: ClassTag[A]
  ): ConfigDescriptor[A] =
    nonEmptyString
      .transformOrFailLeft[A](v => `enum`.withValueEither(v.toByte).swap.map(_.getMessage()).swap)(_.value.toString)

  def charEnum[A <: CharEnumEntry](`enum`: CharEnum[A])(implicit
    ct: ClassTag[A]
  ): ConfigDescriptor[A] =
    nonEmptyString.transformOrFailLeft(v =>
      v.toList match {
        case h :: Nil => `enum`.withValueEither(h).swap.map(_.getMessage()).swap
        case s        => Left(s"Failed to read a character from ${s}")
      }
    )(_.value.toString)

}
