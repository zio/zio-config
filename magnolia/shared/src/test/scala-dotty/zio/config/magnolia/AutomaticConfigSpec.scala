package zio.config.magnolia

import zio.{Config, ConfigProvider}
import zio.config._
import zio.test.Assertion._
import zio.test._

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZoneOffset}
import java.util.UUID
import AutomaticConfigTestUtils._

object AutomaticConfigSpec extends ZIOSpecDefault {

  def spec =
    suite("magnolia spec")(
      test("automatic derivation spec") {
        check(genEnvironment) { environment =>
          val configDesc = deriveConfig[MyConfig]

          val source =
            ConfigProvider.fromMap(environment)

          assertZIO(source.load(configDesc).either)(isRight)
        }
      },
      test("derives syntax spec") {
        check(genEnvironment) { environment =>
          val configDesc = summon[Config[MyConfigDerived]]
          val source     = ConfigProvider.fromMap(environment)

          assertZIO(source.load(configDesc).either)(isRight)
        }
      }
    )
}

object AutomaticConfigTestUtils {

  @discriminator("type")
  sealed trait Credentials

  case class Password(value: String) extends Credentials

  case class Token(value: String) extends Credentials

  case object InstanceProfile extends Credentials

  sealed trait Price

  case class Description(value: String) extends Price

  case class Currency(value: Double) extends Price

  final case class Aws(region: String, security: Credentials)

  final case class DbUrl(value: String)

  final case class MyConfig(
    aws: Aws,
    cost: Price,
    dburl: DbUrl,
    port: Int,
    amount: Option[Long],
    quantity: Either[Long, String],
    default: Int = 1,
    anotherDefault: Boolean = true,
    descriptions: List[String],
    created: LocalDate,
    updated: LocalTime,
    lastVisited: LocalDateTime,
    id: UUID
  )

  final case class MyConfigDerived(
    aws: Aws,
    cost: Price,
    dburl: DbUrl,
    port: Int,
    amount: Option[Long],
    quantity: Either[Long, String],
    default: Int = 1,
    anotherDefault: Boolean = true,
    descriptions: List[String],
    created: LocalDate,
    updated: LocalTime,
    lastVisited: LocalDateTime,
    id: UUID
  ) derives Config

  private val genPriceDescription             = Gen.const(Description("some description"))
  private val genCurrency: Gen[Any, Currency] = Gen.double(10.0, 20.0).map(Currency.apply)
  private val genPrice: Gen[Any, Price]       = Gen.oneOf(genPriceDescription, genCurrency)

  private val genToken           = Gen.const(Token("someToken"))
  private val genPassword        = Gen.const(Password("some passeword"))
  private val genInstanceProfile = Gen.const(InstanceProfile)
  private val genCredentials     = Gen.oneOf(genToken, genPassword, genInstanceProfile)

  private val genDbUrl = Gen.const(DbUrl("dburl"))

  private val genAws =
    for {
      region      <- Gen.const("region")
      credentials <- genCredentials
    } yield Aws(region, credentials)

  private[magnolia] val genEnvironment =
    for {
      aws            <- genAws
      price          <- genPrice
      dbUrl          <- genDbUrl
      port           <- Gen.int
      amount         <- Gen.option(Gen.long(1, 100))
      quantity       <- Gen.either(Gen.long(5, 10), genAlpha)
      default        <- Gen.option(Gen.int)
      anotherDefault <- Gen.option(Gen.boolean)
      descriptions   <- Gen.const("description")
      created        <- genLocalDateString
      updated        <- genLocalTimeString
      lastVisited    <- genLocalDateTimeString
      id             <- Gen.uuid
      partialMyConfig = Map(
                          "aws.region"   -> aws.region,
                          price match {
                            case Description(description) => "cost.Description.value" -> description
                            case Currency(dollars)        => "cost.Currency.value"    -> dollars.toString
                          },
                          "dburl.value"  -> dbUrl.value,
                          "port"         -> port.toString,
                          "quantity"     -> quantity.fold(d => d.toString, s => s),
                          "descriptions" -> descriptions.mkString(","),
                          "created"      -> created,
                          "updated"      -> updated,
                          "lastVisited"  -> lastVisited,
                          "id"           -> id.toString
                        ) ++ (aws.security match {
                          case Password(password) =>
                            Map("aws.security.type" -> "Password", "aws.security.value" -> password)
                          case Token(token)       =>
                            Map("aws.security.type" -> "Token", "aws.security.value" -> token)
                          case InstanceProfile    =>
                            Map("aws.security.type" -> "InstanceProfile")
                        }) ++ amount.map(double => ("amount", double.toString)).toList
    } yield (default, anotherDefault) match {
      case (Some(v1), Some(v2)) => partialMyConfig ++ List(("default", v1.toString), ("anotherDefault", v2.toString))
      case (Some(v1), None)     => partialMyConfig + (("default", v1.toString))
      case (None, Some(v2))     => partialMyConfig + (("anotherDefault", v2.toString))
      case (None, None)         => partialMyConfig
    }

  def genAlpha: Gen[Any, String] =
    for {
      n <- Gen.int(1, 10) // zio-config supports only cons hence starting with 1
      s <- Gen.listOfN(n)(Gen.char(65, 122))
    } yield s.mkString

  val genInstant: Gen[Any, Instant] =
    Gen.long.map(Instant.ofEpochMilli)

  val genLocalDateString: Gen[Any, String] =
    genInstant.map(_.atZone(ZoneOffset.UTC).toLocalDate.toString)

  val genLocalDateTimeString: Gen[Any, String] =
    genInstant.map(_.atZone(ZoneOffset.UTC).toLocalDateTime.toString)

  val genLocalTimeString: Gen[Any, String] =
    genInstant.map(_.atZone(ZoneOffset.UTC).toLocalTime.toString)
}
