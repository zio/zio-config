package zio.config

import _root_.enumeratum._
import _root_.enumeratum.values._
import zio.Config

import scala.reflect.ClassTag

import Config._

package object enumeratum {
  def nonEmptyString: Config[String] =
    string.mapOrFail(str => if (str.isEmpty) Left(Config.Error.InvalidData(message = "Empty string")) else Right(str))

  def `enum`[A <: EnumEntry](`enum`: Enum[A])(implicit ct: ClassTag[A]): Config[A] =
    nonEmptyString.mapOrFail(s =>
      `enum`.withNameEither(s).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
    )

  def intEnum[A <: IntEnumEntry](`enum`: IntEnum[A])(implicit
    ct: ClassTag[A]
  ): Config[A] =
    nonEmptyString
      .mapOrFail(v =>
        `enum`.withValueEither(v.toInt).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
      )

  def longEnum[A <: LongEnumEntry](`enum`: LongEnum[A])(implicit
    ct: ClassTag[A]
  ): Config[A] =
    nonEmptyString
      .mapOrFail[A](v =>
        `enum`.withValueEither(v.toLong).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
      )

  def shortEnum[A <: ShortEnumEntry](`enum`: ShortEnum[A])(implicit
    ct: ClassTag[A]
  ): Config[A] =
    nonEmptyString
      .mapOrFail[A](v =>
        `enum`.withValueEither(v.toShort).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
      )

  def stringEnum[A <: StringEnumEntry](`enum`: StringEnum[A])(implicit
    ct: ClassTag[A]
  ): Config[A] =
    string
      .mapOrFail[A](v =>
        `enum`.withValueEither(v).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
      )

  def byteEnum[A <: ByteEnumEntry](`enum`: ByteEnum[A])(implicit
    ct: ClassTag[A]
  ): Config[A] =
    nonEmptyString
      .mapOrFail[A](v =>
        `enum`.withValueEither(v.toByte).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
      )

  def charEnum[A <: CharEnumEntry](`enum`: CharEnum[A])(implicit
    ct: ClassTag[A]
  ): Config[A] =
    nonEmptyString.mapOrFail(v =>
      v.toList match {
        case h :: Nil =>
          `enum`.withValueEither(h).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
        case s        => Left(Config.Error.InvalidData(message = s"Failed to read a character from ${s}"))
      }
    )

}
