package zio.config.examples

import zio.ZIO
import zio.config.ConfigSource
import zio.config.ConfigDescriptor._
import zio.config.magnolia.ConfigDescriptorProvider._
import zio.console.Console.Live.console._
import zio.config.ConfigDescriptor
import zio.config._

sealed trait Credentials
case class Password(password: String) extends Credentials
case class Token(token: String)       extends Credentials

sealed trait Price
case class Description(description: String) extends Price
case class Currency(dollars: Double)        extends Price

final case class Aws(region: String, credentials: Credentials)

final case class DbUrl(dburl: String) extends AnyVal

final case class MyConfig(
  aws: Aws,
  price: Price,
  dburl: DbUrl,
  port: Int,
  amount: Option[Double],
  quanity: Either[Double, String],
  default: Int = 1,
  anotherDefault: Int = 2
)

object AutomaticConfigDescriptor extends zio.App {
  // Typeclass derivation through Magnolia
  //private val configDesc = description[MyConfig]

  // Non automatic
  val credentialConfig = nested("credentials")(string("password"))
    .orElseEither(nested("credentials")(string("token")))
    .xmap(
      value =>
        value match {
          case Left(value)  => Password(value): Credentials
          case Right(value) => Token(value): Credentials
        }
    )(
      b =>
        b match {
          case Password(password) => Left(password)
          case Token(token)       => Right(token)
        }
    )

  val awsConfig = (string("region") |@| credentialConfig)(Aws.apply, Aws.unapply)

  val priceConfig = string("description")
    .orElseEither(double("dollars"))
    .xmap({
      case Left(str) => Description(str): Price
      case Right(db) => Currency(db): Price
    })(
      b =>
        b match {
          case Description(description) => Left(description)
          case Currency(dollars)        => Right(dollars)
        }
    )

  val myConfigDesc =
    (nested("aws")(awsConfig) |@| nested("price")(priceConfig) |@| string("dburl").xmap(DbUrl)(_.dburl) |@| int("port") |@| double(
      "amount"
    ).optional |@| double(
      "quanity"
    ).orElseEither(string("quanity")) |@| int("default").default(1) |@| int("anotherDefault").default(2))(
      MyConfig.apply,
      MyConfig.unapply
    )

  val map =
    Map(
      "anotherDefault"        -> "956330957",
      "aws.credentials.token" -> "some token",
      "quanity"               -> "7.013515557377486",
      "amount"                -> "84.47017915911125",
      "price.description"     -> "30 dollars",
      "price.dollars"         -> "30.0",
      "port"                  -> "-821114481",
      "aws.region"            -> "6",
      "dburl"                 -> "i"
    )

  sealed trait Price1
  case class Currency(dollars: Double)         extends Price1
  case class Description1(description: String) extends Price1

  final case class Narrowed(price: Price1)
  val narrowedDescription: ConfigDescriptor[String, String, Narrowed] = description[Narrowed]

  val thisisObtained =
    Narrowed(Currency1(30.0))

  private val source =
    ConfigSource.fromMap(map)

  private val config = narrowedDescription from source

  import zio.config._

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    read(config).foldM(
      r => putStrLn(r.mkString(",")) *> ZIO.succeed(1),
      result => putStrLn(result.toString()) *> ZIO.succeed(0)
      //putStrLn(write(config, result).map(_.flattenString()).toString()) *> ZIO.succeed(0)
    )

  //
  // Read output:
  //=============
  //  MyConfig(
  //    Aws(us-east, Token(some token)),
  //    Currency(50.0),
  //    DbUrl(some url),
  //    1,
  //    Some(3.14),
  //    Right(30 kilos),
  //    1,
  //    3
  //  )

  // Write output:
  // Right(
  // HashMap(
  //   anotherDefault -> 3,
  //   price.dollars -> 50.0,
  //   amount -> 3.14,
  //   aws.credentials.token -> some token,
  //   port -> 1,
  //   aws.region -> us-east,
  //   dburl -> some url,
  //   default -> 1,
  //   quanity -> 30 kilos)
  // )
  //
  // Process finished with exit code 0
  //
}

object Something extends scala.App {

  final case class MyConfig(price: Price)

  val automatic: ConfigDescriptor[String, String, MyConfig] =
    description[MyConfig]

  val priceConfig = nested("price")(double("dollars"))
    .orElseEither(nested("price")(string("description")))
    .xmap({
      case Left(value) => Currency(value): Price
      case Right(desc) => Description(desc): Price
    })(
      b =>
        b match {
          case Description(description) => Right(description)
          case Currency(dollars)        => Left(dollars)
        }
    )

  val goodManual =
    priceConfig.xmap(MyConfig)(_.price)

  val badManual = {
    val descriptionDescription = string("description")
      .xmap(Description)(_.description)

    val currencyDescription =
      double("dollars")
        .xmap(Currency)(_.dollars)

    val desc =
      currencyDescription
        .orElseEither(descriptionDescription)
        .xmap(r => r.fold(r => r: Price, v => v: Price))(
          price =>
            price match {
              case Currency(value)          => Left(Currency(value))
              case Description(description) => Right(Description(description))
            }
        )

    desc.xmap(MyConfig)(_.price)

  }

  //println(automatic)

  val thisisObtained =
    MyConfig(Description("sdsds"))

  val result = write(badManual, thisisObtained).map(_.flattenString())

  println(result)

}
