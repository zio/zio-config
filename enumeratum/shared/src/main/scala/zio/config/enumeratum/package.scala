package zio.config

import _root_.enumeratum._
import _root_.enumeratum.values._
import zio.Config

import scala.reflect.ClassTag

import Config._

/**
 * Descriptions that read enumeratum enums: an `Enum` entry by its name, or a value enum entry (`IntEnum`, `LongEnum`,
 * `ShortEnum`, `StringEnum`, `ByteEnum`, `CharEnum`) by its value. They take the enum's companion and need no
 * derivation.
 *
 * After `import zio.config._`, the name `enumeratum` refers to this package and hides the library's own `enumeratum`
 * package, so write the enumeratum imports with `_root_`, as in `import _root_.enumeratum.EnumEntry`.
 *
 * @see
 *   [[https://zio.dev/zio-config/integrations/ Integrations]]
 */
package object enumeratum {

  /**
   * Reads a string and fails with `Config.Error.InvalidData` and the message `Empty string` when it is empty. A string
   * of spaces is not empty and is accepted.
   */
  def nonEmptyString: Config[String] =
    string.mapOrFail(str => if (str.isEmpty) Left(Config.Error.InvalidData(message = "Empty string")) else Right(str))

  /**
   * Reads an entry of `enum` by its name (`entryName`), which must match exactly, including case. An empty value fails
   * with the message `Empty string`, and a name that matches no entry fails with enumeratum's message, such as
   * `Blue is not a member of Enum (Red, Green)`. `enum` is a keyword in Scala 3, so write the name with backticks.
   *
   * {{{
   * import zio._
   * import zio.config.enumeratum._
   * import _root_.enumeratum.{Enum, EnumEntry}
   *
   * sealed trait Color extends EnumEntry
   *
   * object Color extends Enum[Color] {
   *   val values = findValues
   *
   *   case object Red   extends Color
   *   case object Green extends Color
   * }
   *
   * val color: Config[Color] = `enum`(Color).nested("color")
   *
   * val loaded: IO[Config.Error, Color] = ConfigProvider.fromMap(Map("color" -> "Green")).load(color)
   * // succeeds with Green
   * }}}
   */
  def `enum`[A <: EnumEntry](`enum`: Enum[A])(implicit ct: ClassTag[A]): Config[A] =
    nonEmptyString.mapOrFail(s =>
      `enum`.withNameEither(s).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
    )

  /**
   * Reads an entry of the `IntEnum` by its value, parsed as an `Int`. A number that matches no entry fails with
   * enumeratum's message, such as `3 is not a member of ValueEnum (1, 5)`. A value that is not a number, or is out of
   * range for `Int`, makes the load die with a `java.lang.NumberFormatException` instead of failing with a
   * `Config.Error`.
   *
   * {{{
   * import zio._
   * import zio.config.enumeratum._
   * import _root_.enumeratum.values.{IntEnum, IntEnumEntry}
   *
   * sealed abstract class Priority(val value: Int) extends IntEnumEntry
   *
   * object Priority extends IntEnum[Priority] {
   *   val values: IndexedSeq[Priority] = IndexedSeq(Low, High)
   *
   *   case object Low  extends Priority(1)
   *   case object High extends Priority(5)
   * }
   *
   * val priority: Config[Priority] = intEnum(Priority).nested("priority")
   *
   * val loaded: IO[Config.Error, Priority] = ConfigProvider.fromMap(Map("priority" -> "5")).load(priority)
   * // succeeds with High
   * }}}
   */
  def intEnum[A <: IntEnumEntry](`enum`: IntEnum[A])(implicit
    ct: ClassTag[A]
  ): Config[A] =
    nonEmptyString
      .mapOrFail(v =>
        `enum`.withValueEither(v.toInt).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
      )

  /**
   * Reads an entry of the `LongEnum` by its value, parsed as a `Long`. A number that matches no entry fails with
   * enumeratum's message. A value that is not a number, or is out of range for `Long`, makes the load die with a
   * `java.lang.NumberFormatException` instead of failing with a `Config.Error`.
   *
   * {{{
   * import zio._
   * import zio.config.enumeratum._
   * import _root_.enumeratum.values.{LongEnum, LongEnumEntry}
   *
   * sealed abstract class Quota(val value: Long) extends LongEnumEntry
   *
   * object Quota extends LongEnum[Quota] {
   *   val values: IndexedSeq[Quota] = IndexedSeq(Free, Paid)
   *
   *   case object Free extends Quota(1000L)
   *   case object Paid extends Quota(5000000000L)
   * }
   *
   * val quota: Config[Quota] = longEnum(Quota).nested("quota")
   *
   * val loaded: IO[Config.Error, Quota] = ConfigProvider.fromMap(Map("quota" -> "5000000000")).load(quota)
   * // succeeds with Paid
   * }}}
   */
  def longEnum[A <: LongEnumEntry](`enum`: LongEnum[A])(implicit
    ct: ClassTag[A]
  ): Config[A] =
    nonEmptyString
      .mapOrFail[A](v =>
        `enum`.withValueEither(v.toLong).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
      )

  /**
   * Reads an entry of the `ShortEnum` by its value, parsed as a `Short`. A number that matches no entry fails with
   * enumeratum's message. A value that is not a number, or is out of range for `Short`, makes the load die with a
   * `java.lang.NumberFormatException` instead of failing with a `Config.Error`.
   *
   * {{{
   * import zio._
   * import zio.config.enumeratum._
   * import _root_.enumeratum.values.{ShortEnum, ShortEnumEntry}
   *
   * sealed abstract class HttpPort(val value: Short) extends ShortEnumEntry
   *
   * object HttpPort extends ShortEnum[HttpPort] {
   *   val values: IndexedSeq[HttpPort] = IndexedSeq(Plain, Secure)
   *
   *   case object Plain  extends HttpPort(80)
   *   case object Secure extends HttpPort(443)
   * }
   *
   * val port: Config[HttpPort] = shortEnum(HttpPort).nested("port")
   *
   * val loaded: IO[Config.Error, HttpPort] = ConfigProvider.fromMap(Map("port" -> "443")).load(port)
   * // succeeds with Secure
   * }}}
   */
  def shortEnum[A <: ShortEnumEntry](`enum`: ShortEnum[A])(implicit
    ct: ClassTag[A]
  ): Config[A] =
    nonEmptyString
      .mapOrFail[A](v =>
        `enum`.withValueEither(v.toShort).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
      )

  /**
   * Reads an entry of the `StringEnum` by its value. Unlike the other functions in this package, it accepts an empty
   * value, which matches an entry whose value is the empty string. A value that matches no entry fails with
   * enumeratum's message, such as `trace is not a member of ValueEnum (warn, info)`.
   *
   * {{{
   * import zio._
   * import zio.config.enumeratum._
   * import _root_.enumeratum.values.{StringEnum, StringEnumEntry}
   *
   * sealed abstract class LogLevel(val value: String) extends StringEnumEntry
   *
   * object LogLevel extends StringEnum[LogLevel] {
   *   val values: IndexedSeq[LogLevel] = IndexedSeq(Warn, Info)
   *
   *   case object Warn extends LogLevel("warn")
   *   case object Info  extends LogLevel("info")
   * }
   *
   * val level: Config[LogLevel] = stringEnum(LogLevel).nested("level")
   *
   * val loaded: IO[Config.Error, LogLevel] = ConfigProvider.fromMap(Map("level" -> "info")).load(level)
   * // succeeds with Info
   * }}}
   */
  def stringEnum[A <: StringEnumEntry](`enum`: StringEnum[A])(implicit
    ct: ClassTag[A]
  ): Config[A] =
    string
      .mapOrFail[A](v =>
        `enum`.withValueEither(v).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
      )

  /**
   * Reads an entry of the `ByteEnum` by its value, parsed as a `Byte`. A number that matches no entry fails with
   * enumeratum's message. A value that is not a number, or is out of range for `Byte`, makes the load die with a
   * `java.lang.NumberFormatException` instead of failing with a `Config.Error`. For example, `300` dies.
   *
   * {{{
   * import zio._
   * import zio.config.enumeratum._
   * import _root_.enumeratum.values.{ByteEnum, ByteEnumEntry}
   *
   * sealed abstract class Version(val value: Byte) extends ByteEnumEntry
   *
   * object Version extends ByteEnum[Version] {
   *   val values: IndexedSeq[Version] = IndexedSeq(V1, V2)
   *
   *   case object V1 extends Version(1)
   *   case object V2 extends Version(2)
   * }
   *
   * val version: Config[Version] = byteEnum(Version).nested("version")
   *
   * val loaded: IO[Config.Error, Version] = ConfigProvider.fromMap(Map("version" -> "2")).load(version)
   * // succeeds with V2
   * }}}
   */
  def byteEnum[A <: ByteEnumEntry](`enum`: ByteEnum[A])(implicit
    ct: ClassTag[A]
  ): Config[A] =
    nonEmptyString
      .mapOrFail[A](v =>
        `enum`.withValueEither(v.toByte).swap.map(v => Config.Error.InvalidData(message = v.getMessage())).swap
      )

  /**
   * Reads an entry of the `CharEnum` by its value, which must be exactly one character. A longer value fails with a
   * message such as `Failed to read a character from List(A, B)`, and a character that matches no entry fails with
   * enumeratum's message.
   *
   * {{{
   * import zio._
   * import zio.config.enumeratum._
   * import _root_.enumeratum.values.{CharEnum, CharEnumEntry}
   *
   * sealed abstract class Grade(val value: Char) extends CharEnumEntry
   *
   * object Grade extends CharEnum[Grade] {
   *   val values: IndexedSeq[Grade] = IndexedSeq(A, B)
   *
   *   case object A extends Grade('A')
   *   case object B extends Grade('B')
   * }
   *
   * val grade: Config[Grade] = charEnum(Grade).nested("grade")
   *
   * val loaded: IO[Config.Error, Grade] = ConfigProvider.fromMap(Map("grade" -> "B")).load(grade)
   * // succeeds with B
   * }}}
   */
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
