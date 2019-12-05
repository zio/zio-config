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
  def iPv4[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, IPv4]
  ): ConfigDescriptor[K, V, Refined[A, IPv4]] =
    asRefined[K, V, A, IPv4](desc)

  /** Predicate that checks if a `String` is a valid IPv6 */
  def iPv6[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, IPv6]
  ): ConfigDescriptor[K, V, Refined[A, IPv6]] =
    asRefined[K, V, A, IPv6](desc)

  /** Predicate that checks if a `String` matches the regular expression `S` */
  def matchesRegex[S]: MatchesRegexPartiallyApplied[S] =
    new MatchesRegexPartiallyApplied[S]

  /** Predicate that checks if a `String` is a valid regular expression */
  def regex[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Regex]
  ): ConfigDescriptor[K, V, Refined[A, Regex]] =
    asRefined[K, V, A, Regex](desc)

  /** Predicate that checks if a `String` starts with the prefix `S` */
  def startsWith[S]: StartsWithPartiallyApplied[S] =
    new StartsWithPartiallyApplied[S]

  /** Predicate that checks if a `String` is a valid URI */
  def uri[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Uri]
  ): ConfigDescriptor[K, V, Refined[A, Uri]] =
    asRefined[K, V, A, Uri](desc)

  /** Predicate that checks if a `String` is a valid URL */
  def url[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Url]
  ): ConfigDescriptor[K, V, Refined[A, Url]] =
    asRefined[K, V, A, Url](desc)

  /** Predicate that checks if a `String` is a valid UUID */
  def uuid[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Uuid]
  ): ConfigDescriptor[K, V, Refined[A, Uuid]] =
    asRefined[K, V, A, Uuid](desc)

  /** Predicate that checks if a `String` is a parsable `Byte` */
  def validByte[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, ValidByte]
  ): ConfigDescriptor[K, V, Refined[A, ValidByte]] =
    asRefined[K, V, A, ValidByte](desc)

  /** Predicate that checks if a `String` is a parsable `Short` */
  def validShort[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, ValidShort]
  ): ConfigDescriptor[K, V, Refined[A, ValidShort]] =
    asRefined[K, V, A, ValidShort](desc)

  /** Predicate that checks if a `String` is a parsable `Int` */
  def validInt[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, ValidInt]
  ): ConfigDescriptor[K, V, Refined[A, ValidInt]] =
    asRefined[K, V, A, ValidInt](desc)

  /** Predicate that checks if a `String` is a parsable `Long` */
  def validLong[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, ValidLong]
  ): ConfigDescriptor[K, V, Refined[A, ValidLong]] =
    asRefined[K, V, A, ValidLong](desc)

  /** Predicate that checks if a `String` is a parsable `Float` */
  def validFloat[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, ValidFloat]
  ): ConfigDescriptor[K, V, Refined[A, ValidFloat]] =
    asRefined[K, V, A, ValidFloat](desc)

  /** Predicate that checks if a `String` is a parsable `Double` */
  def validDouble[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, ValidDouble]
  ): ConfigDescriptor[K, V, Refined[A, ValidDouble]] =
    asRefined[K, V, A, ValidDouble](desc)

  /** Predicate that checks if a `String` is a parsable `BigInt` */
  def validBigInt[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, ValidBigInt]
  ): ConfigDescriptor[K, V, Refined[A, ValidBigInt]] =
    asRefined[K, V, A, ValidBigInt](desc)

  /** Predicate that checks if a `String` is a parsable `BigDecimal` */
  def validBigDecimal[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, ValidBigDecimal]
  ): ConfigDescriptor[K, V, Refined[A, ValidBigDecimal]] =
    asRefined[K, V, A, ValidBigDecimal](desc)

  /** Predicate that checks if a `String` is well-formed XML */
  def xml[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Xml]
  ): ConfigDescriptor[K, V, Refined[A, Xml]] =
    asRefined[K, V, A, Xml](desc)

  /** Predicate that checks if a `String` is a valid XPath expression */
  def xPath[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, XPath]
  ): ConfigDescriptor[K, V, Refined[A, XPath]] =
    asRefined[K, V, A, XPath](desc)

  /** Predicate that checks if a `String` has no leading or trailing whitespace */
  def trimmed[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Trimmed]
  ): ConfigDescriptor[K, V, Refined[A, Trimmed]] =
    asRefined[K, V, A, Trimmed](desc)

}
