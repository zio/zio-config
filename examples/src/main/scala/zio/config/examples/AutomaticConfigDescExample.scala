package zio.config.examples

import zio.ZIO
import zio.config.ConfigSource
import zio.config.magnolia.ConfigDescriptorProvider._
import zio.config.ConfigDescriptor._
import zio.console.Console.Live.console._

sealed trait Credentials
case class Password(password: String) extends Credentials
case class Token(token: String)       extends Credentials

sealed trait Price
case class Description(description: String) extends Price
case class Currency(dollars: Double)        extends Price

//final case class Aws(region: String, credentials: Credentials)
final case class Aws(credentials: Credentials)

final case class DbUrl(value: String) extends AnyVal

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

  val password =
    string("password").xmap(Password)(_.password)

  val token =
    string("token").xmap(Token)(_.token)

  val credentials =
    password
      .orElseEither(token)
      .xmap({
        case Left(pass) => pass: Credentials
        case Right(tok) => tok: Credentials
      })(
        credential =>
          credential match {
            case a @ Password(password) => Left(a)
            case b @ Token(password)    => Right(b)
          }
      )

  val priceDescription =
    string("description").xmap(Description)(_.description)

  val currency =
    double("dollars").xmap(Currency)(_.dollars)

  val price =
    priceDescription
      .orElseEither(currency)
      .xmap({
        case Left(pass) => pass: Price
        case Right(tok) => tok: Price
      })(
        credential =>
          credential match {
            case a @ Description(_) => Left(a)
            case b @ Currency(_)    => Right(b)
          }
      )

  // val aws = (string("region") |@| nested("credentials")(credentials))(Aws.apply, Aws.unapply)
  val aws = nested("credentials")(credentials).xmap(Aws)(_.credentials)
  val nonAutomaticConfig =
    (nested("aws")(aws) |@| nested("price")(price) |@| string("dburl")
      .xmap(DbUrl)(_.value)
      |@| int("port") |@| double(
      "amount"
    ).optional |@| double(
      "quanity"
    ).orElseEither(string("quanity")) |@| int("default").default(1) |@| int("anotherDefault").default(12))(
      MyConfig,
      MyConfig.unapply
    )

  // Typeclass derivation through Magnolia
  private val automaticConfig = description[MyConfig]

  private val source =
    ConfigSource.fromMap(
      Map(
        "aws.region"            -> "us-east",
        "aws.credentials.token" -> "password",
        "port"                  -> "1",
        "dburl"                 -> "some url",
        "amount"                -> "3.14",
        "quanity"               -> "30 kilos",
        "price.description"     -> "1000.0",
        "anotherDefault"        -> "3"
      )
    )

  private val config = nonAutomaticConfig from source

  import zio.config._

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    read(config).foldM(
      r => putStrLn(r.mkString(",")) *> ZIO.succeed(1),
      result =>
        putStrLn(result.toString()) *> putStrLn(write(config, result).toString()) *>
          ZIO.succeed(0)
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
  //
  // Write output:
  // =============
  // Right(
  //  Record(
  //    HashMap(
  //      anotherDefault -> Leaf(3),
  //      aws ->
  //        Record(
  //          HashMap(
  //            token -> Leaf(some token),
  //            region -> Leaf(us-east)
  //          )
  //        ),
  //      port2 -> Leaf(3.14),
  //      price -> Leaf(30 euros),
  //      default -> Leaf(1),
  //      dburl -> Leaf(some url),
  //      price2 -> Leaf(50.0),
  //      port -> Leaf(1)
  //    )
  //  )
  //
  // Process finished with exit code 0
  //
}
