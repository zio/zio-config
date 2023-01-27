package zio.config.testsupport

import zio.config.{Config, _}
import zio.test.Gen.alphaNumericChar
import zio.test.{Gen, Sized}

import Config._

object MapConfigTestSupport {
  def genAppConfig(stringGen: Gen[Sized, String] = stringN(1, 15)): Gen[Sized, AppConfig] =
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
      val description: Config[AwsConfig] =
        head("aws")(
          (head("key")(string) zip head("secret")(string) zip KinesisConfig.description).to[AwsConfig]
        )
    }

    final case class KinesisConfig(inputTopic: String)

    object KinesisConfig {
      val description: Config[KinesisConfig] =
        (head("kinesis")(head("inputtopic")(string)).to[KinesisConfig])
    }

    final case class PubSubConfig(outputTopic: String)

    object PubSubConfig {
      val description: Config[PubSubConfig] =
        head("ps")(head("outputtopic")(string).to[PubSubConfig])
    }

    val descriptor: Config[AppConfig] =
      head("SystemF")(
        (AwsConfig.description zip AppConfig.PubSubConfig.description zip JobConfig.descriptor).to[AppConfig]
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
    val descriptor: Config[DataflowConfig] =
      head("df")(
        ((head("name")(string)) zip
          head("project")(string) zip
          head("region")(string) zip
          head("zone")(string) zip
          head("subnet")(string) zip
          head("gcptemplocation")(string)).to[DataflowConfig]
      )
  }

  ////

  final case class JobConfig(
    dataflowConfig: Option[DataflowConfig],
    supervise: Boolean
  )

  object JobConfig {
    val descriptor: Config[JobConfig] =
      head("job")(
        (DataflowConfig.descriptor.optional zip head("supervise")(boolean)).to[JobConfig]
      )
  }

  def stringN(min: Int, max: Int): Gen[Sized, String] =
    for {
      n <- Gen.int(min, max)
      s <- Gen.stringN(n)(alphaNumericChar)
    } yield s

  def stringNWithInjector(min: Int, max: Int, inj: String): Gen[Sized, String] =
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
