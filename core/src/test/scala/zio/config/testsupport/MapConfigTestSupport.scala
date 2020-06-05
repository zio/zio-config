package zio.config.testsupport

import zio.config.ConfigDescriptor, ConfigDescriptor._
import zio.random.Random
import zio.test.Gen.alphaNumericChar
import zio.test.{ Gen, Sized }

object MapConfigTestSupport {
  def genAppConfig(stringGen: Gen[Random with Sized, String] = stringN(1, 15)): Gen[Random with Sized, AppConfig] =
    for {
      strings   <- Gen.listOfN(10)(stringGen)
      supervise <- Gen.boolean
    } yield AppConfig(
      awsConfig = AppConfig.AwsConfig(strings.head, strings(1), AppConfig.KinesisConfig(strings(2))),
      pubSubConfig = AppConfig.PubSubConfig(strings(9)),
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
      )
    )

  final case class AppConfig(
    awsConfig: AppConfig.AwsConfig,
    pubSubConfig: AppConfig.PubSubConfig,
    jobConfig: JobConfig
  )

  object AppConfig {
    final case class AwsConfig(key: String, secret: String, kinesisConfig: KinesisConfig)

    object AwsConfig {
      val description: ConfigDescriptor[AwsConfig] =
        head("aws")(
          (head("key")(string) |@| head("secret")(string) |@| KinesisConfig.description)(
            AwsConfig.apply,
            AwsConfig.unapply
          )
        )
    }

    final case class KinesisConfig(inputTopic: String)

    object KinesisConfig {
      val description =
        (head("kinesis")(head("inputtopic")(string))(KinesisConfig.apply, KinesisConfig.unapply))
    }

    final case class PubSubConfig(outputTopic: String)

    object PubSubConfig {
      val description =
        head("ps")(head("outputtopic")(string)(PubSubConfig.apply, PubSubConfig.unapply))
    }

    val descriptor: ConfigDescriptor[AppConfig] =
      head("SystemF")(
        (AwsConfig.description |@| AppConfig.PubSubConfig.description |@| JobConfig.descriptor)(
          AppConfig.apply,
          AppConfig.unapply
        )
      )
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
    val descriptor: ConfigDescriptor[DataflowConfig] =
      head("df")(
        ((head("name")(string)) |@|
          head("project")(string) |@|
          head("region")(string) |@|
          head("zone")(string) |@|
          head("subnet")(string) |@|
          head("gcptemplocation")(string))(DataflowConfig.apply, DataflowConfig.unapply)
      )
  }

  ////

  final case class JobConfig(
    dataflowConfig: Option[DataflowConfig],
    supervise: Boolean
  )

  object JobConfig {
    val descriptor: ConfigDescriptor[JobConfig] =
      head("job")(
        (DataflowConfig.descriptor.optional |@| head("supervise")(boolean))(JobConfig.apply, JobConfig.unapply)
      )
  }

  def stringN(min: Int, max: Int): Gen[Random with Sized, String] =
    for {
      n <- Gen.int(min, max)
      s <- Gen.stringN(n)(alphaNumericChar)
    } yield s

  def stringNWithInjector(min: Int, max: Int, inj: String): Gen[Random with Sized, String] =
    for {
      length  <- Gen.int(min, max)
      index   <- Gen.int(0, length - 1)
      decider <- Gen.boolean
      text    <- Gen.stringN(length)(alphaNumericChar).map(s => injectString(s, inj, index, decider))
    } yield text

  def injectString(text: String, inject: String, index: Int, randomizer: Boolean): String =
    if (randomizer) {
      val (start, end) = text.splitAt(index)
      s"$start$inject$end"
    } else text
}
