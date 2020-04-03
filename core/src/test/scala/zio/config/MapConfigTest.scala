package zio.config

import zio.config.ConfigDescriptor.{ boolean, nested, string }
import zio.config.ReadError.ConversionError
import zio.random.Random
import zio.test.Assertion._
import zio.test.Gen.alphaNumericChar
import zio.test.environment.TestEnvironment
import zio.test.{ DefaultRunnableSpec, _ }
import zio.{ IO, ZIO }

object MapConfigTest extends DefaultRunnableSpec {
  def spec: Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("Configuration from command-line-style arguments")(
      testM("Configuration from arguments roundtrip") {
        checkM(genAppConfig) { appConfig =>
          val p2: zio.IO[ReadError[String], AppConfig] =
            for {
              args   <- toMap(AppConfig.descriptor, appConfig)
              reread <- AppConfig.fromMap(args)
            } yield reread.get

          assertM(p2.either)(isRight(equalTo(appConfig)))
        }
      }
    )

  def genAppConfig: Gen[Random with Sized, AppConfig] =
    for {
      strings   <- Gen.listOfN(10)(stringN(1, 15))
      supervise <- Gen.boolean
    } yield AppConfig(
      awsKey = strings(0),
      awsSecret = strings(1),
      inputTopic = strings(2),
      jobConfig = JobConfig(
        dataflowConfig = Some(
          DataflowConfig(
            name = strings(3),
            project = strings(4),
            region = strings(5),
            zone = strings(6),
            subnet = strings(7),
            gcpTempLocation = strings(8)
          )
        ),
        supervise = supervise
      ),
      outputTopic = strings(9)
    )

  final case class AppConfig(
    awsKey: String,
    awsSecret: String,
    inputTopic: String,
    jobConfig: JobConfig,
    outputTopic: String
  )

  object AppConfig {
    def fromMap(args: Map[String, String]): ZIO[Any, ReadError[String], Config[AppConfig]] =
      ZIO.environment.provideLayer(Config.fromMap(args, descriptor, "WTL", None, None))

    private val prefix: String        = "SystemF"
    private val prefixAws: String     = concat(prefix, "aws")
    private val prefixKinesis: String = concat(prefixAws, "kinesis")
    private val prefixPubsub: String  = concat(prefix, "ps")

    val descriptor: ConfigDescriptor[String, String, AppConfig] =
      (
        string(concat(prefixAws, "key")) |@|
          string(concat(prefixAws, "secret")) |@|
          string(concat(prefixKinesis, "input_topic")) |@|
          nested(prefix) { JobConfig.descriptor } |@|
          string(concat(prefixPubsub, "output_topic"))
      )(AppConfig.apply, AppConfig.unapply)
  }

  final case class DataflowConfig(
    name: String,
    project: String,
    region: String,
    zone: String,
    subnet: String,
    gcpTempLocation: String
  )

  object DataflowConfig {
    private val prefix = "df"

    val descriptor: ConfigDescriptor[String, String, DataflowConfig] =
      (
        string(concat(prefix, "name")) |@|
          string(concat(prefix, "project")) |@|
          string(concat(prefix, "region")) |@|
          string(concat(prefix, "zone")) |@|
          string(concat(prefix, "subnet")) |@|
          string(concat(prefix, "gcp_temp_location"))
      )(DataflowConfig.apply, DataflowConfig.unapply)
  }

  ////

  final case class JobConfig(
    dataflowConfig: Option[DataflowConfig],
    supervise: Boolean
  )

  object JobConfig {
    private val prefix = "job"

    val descriptor: ConfigDescriptor[String, String, JobConfig] =
      (
        nested(prefix)(DataflowConfig.descriptor).optional |@|
          boolean(concat(prefix, "supervise"))
      )(JobConfig.apply, JobConfig.unapply)
  }

  val separator: String = "_"

  def concat(ss: String*): String =
    ss.mkString(separator)

  def stringN(min: Int, max: Int): Gen[Random with Sized, String] =
    for {
      n <- Gen.int(min, max)
      s <- Gen.stringN(n)(alphaNumericChar)
    } yield s

  def toMap[A](
    descriptor: ConfigDescriptor[String, String, A],
    a: A
  ): ZIO[Any, ReadError[String], Map[String, String]] =
    IO.fromEither(write(descriptor, a))
      .bimap(
        s => ConversionError[String](Vector(Left(0)), s),
        propertyTreeArgs
      )
      .map(tuples => Map(tuples : _*))

  def propertyTreeArgs(propertyTree: PropertyTree[String, String]): List[(String, String)] =
    propertyTree.flatten.toList.map { t: (Vector[String], ::[String]) =>
      (s"${t._1.mkString(separator)}", s"${t._2.mkString(separator)}")
    }

}
