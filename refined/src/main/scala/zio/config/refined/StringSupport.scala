package zio.config.refined

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.string._
import zio.config.ConfigDescriptor
import zio.config.refined.internal._

private[refined] trait StringSupport {

  /** Predicate that checks if a `String` ends with the suffix `S` */
  def endsWith[S]: EndsWithPartiallyApplied[S] =
    new EndsWithPartiallyApplied[S]

  /** Predicate that checks if a `String` is a valid IPv4 */
  def iPv4[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, IPv4]
  ): ConfigDescriptor[Refined[A, IPv4]] =
    asRefined[A, IPv4](desc)

  /** Predicate that checks if a `String` is a valid IPv6 */
  def iPv6[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, IPv6]
  ): ConfigDescriptor[Refined[A, IPv6]] =
    asRefined[A, IPv6](desc)

  /** Predicate that checks if a `String` matches the regular expression `S` */
  def matchesRegex[S]: MatchesRegexPartiallyApplied[S] =
    new MatchesRegexPartiallyApplied[S]

  /** Predicate that checks if a `String` is a valid regular expression */
  def regex[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Regex]
  ): ConfigDescriptor[Refined[A, Regex]] =
    asRefined[A, Regex](desc)

  /** Predicate that checks if a `String` starts with the prefix `S` */
  def startsWith[S]: StartsWithPartiallyApplied[S] =
    new StartsWithPartiallyApplied[S]

  /** Predicate that checks if a `String` is a valid URI */
  def uri[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Uri]
  ): ConfigDescriptor[Refined[A, Uri]] =
    asRefined[A, Uri](desc)

  /** Predicate that checks if a `String` is a valid URL */
  def url[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Url]
  ): ConfigDescriptor[Refined[A, Url]] =
    asRefined[A, Url](desc)

  /** Predicate that checks if a `String` is a valid UUID */
  def uuid[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Uuid]
  ): ConfigDescriptor[Refined[A, Uuid]] =
    asRefined[A, Uuid](desc)

  /** Predicate that checks if a `String` is a parsable `Byte` */
  def validByte[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, ValidByte]
  ): ConfigDescriptor[Refined[A, ValidByte]] =
    asRefined[A, ValidByte](desc)

  /** Predicate that checks if a `String` is a parsable `Short` */
  def validShort[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, ValidShort]
  ): ConfigDescriptor[Refined[A, ValidShort]] =
    asRefined[A, ValidShort](desc)

  /** Predicate that checks if a `String` is a parsable `Int` */
  def validInt[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, ValidInt]
  ): ConfigDescriptor[Refined[A, ValidInt]] =
    asRefined[A, ValidInt](desc)

  /** Predicate that checks if a `String` is a parsable `Long` */
  def validLong[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, ValidLong]
  ): ConfigDescriptor[Refined[A, ValidLong]] =
    asRefined[A, ValidLong](desc)

  /** Predicate that checks if a `String` is a parsable `Float` */
  def validFloat[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, ValidFloat]
  ): ConfigDescriptor[Refined[A, ValidFloat]] =
    asRefined[A, ValidFloat](desc)

  /** Predicate that checks if a `String` is a parsable `Double` */
  def validDouble[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, ValidDouble]
  ): ConfigDescriptor[Refined[A, ValidDouble]] =
    asRefined[A, ValidDouble](desc)

  /** Predicate that checks if a `String` is a parsable `BigInt` */
  def validBigInt[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, ValidBigInt]
  ): ConfigDescriptor[Refined[A, ValidBigInt]] =
    asRefined[A, ValidBigInt](desc)

  /** Predicate that checks if a `String` is a parsable `BigDecimal` */
  def validBigDecimal[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, ValidBigDecimal]
  ): ConfigDescriptor[Refined[A, ValidBigDecimal]] =
    asRefined[A, ValidBigDecimal](desc)

  /** Predicate that checks if a `String` is well-formed XML */
  def xml[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Xml]
  ): ConfigDescriptor[Refined[A, Xml]] =
    asRefined[A, Xml](desc)

  /** Predicate that checks if a `String` is a valid XPath expression */
  def xPath[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, XPath]
  ): ConfigDescriptor[Refined[A, XPath]] =
    asRefined[A, XPath](desc)

  /** Predicate that checks if a `String` has no leading or trailing whitespace */
  def trimmed[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Trimmed]
  ): ConfigDescriptor[Refined[A, Trimmed]] =
    asRefined[A, Trimmed](desc)

}
