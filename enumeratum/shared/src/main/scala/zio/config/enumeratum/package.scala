package zio.config

import scala.reflect.ClassTag

import _root_.enumeratum._
import _root_.enumeratum.values._
import ConfigDescriptor._

package object enumeratum {
  def nonEmptyString: ConfigDescriptor[String] =
    string.transformOrFailLeft(str => if (str.isEmpty) Left("Empty string") else Right(str))(identity)

  def enum[A <: EnumEntry](enum: Enum[A])(implicit ct: ClassTag[A]): ConfigDescriptor[A] =
    nonEmptyString.transformOrFailLeft(s => enum.withNameEither(s).swap.map(_.getMessage()).swap)(_.entryName)

  def intEnum[A <: IntEnumEntry](enum: IntEnum[A])(implicit
    ct: ClassTag[A]
  ): ConfigDescriptor[A] =
    nonEmptyString
      .transformOrFailLeft(v => enum.withValueEither(v.toInt).swap.map(_.getMessage()).swap)(_.value.toString)

  def longEnum[A <: LongEnumEntry](enum: LongEnum[A])(implicit
    ct: ClassTag[A]
  ): ConfigDescriptor[A] =
    nonEmptyString
      .transformOrFailLeft[A](v => enum.withValueEither(v.toLong).swap.map(_.getMessage()).swap)(_.value.toString)
}
