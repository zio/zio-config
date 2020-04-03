package zio.config.testsupport

import zio.config.ConfigDescriptor
import zio.config.ConfigDescriptor.{ boolean, nested, string }
import zio.random.Random
import zio.test.Gen.alphaNumericChar
import zio.test.{ Gen, Sized }

object MapConfigTestSupport {
  def genAppConfig: Gen[Random with Sized, AppConfig] =
    for {
      strings   <- Gen.listOfN(10)(stringN(1, 15))
      supervise <- Gen.boolean
    } yield AppConfig(
      awsConfig = AppConfig.AwsConfig(strings(0), strings(1), AppConfig.KinesisConfig(strings(2))),
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
      val description: ConfigDescriptor[String, String, AwsConfig] =
        nested("aws")(
          (string("key") |@| string("secret") |@| KinesisConfig.description)(AwsConfig.apply, AwsConfig.unapply)
        )
    }

    final case class KinesisConfig(inputTopic: String)

    object KinesisConfig {
      val description =
        (nested("kinesis")(string("inputtopic"))(KinesisConfig.apply, KinesisConfig.unapply))
    }

    final case class PubSubConfig(outputTopic: String)

    object PubSubConfig {
      val description =
        nested("ps")((string("outputtopic"))(PubSubConfig.apply, PubSubConfig.unapply))
    }

    val descriptor: ConfigDescriptor[String, String, AppConfig] =
      nested("SystemF")(
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
    val descriptor: ConfigDescriptor[String, String, DataflowConfig] =
      nested("df")(
        ((string("name")) |@|
          string("project") |@|
          string("region") |@|
          string("zone") |@|
          string("subnet") |@|
          string("gcptemplocation"))(DataflowConfig.apply, DataflowConfig.unapply)
      )
  }

  ////

  final case class JobConfig(
    dataflowConfig: Option[DataflowConfig],
    supervise: Boolean
  )

  object JobConfig {
    val descriptor: ConfigDescriptor[String, String, JobConfig] =
      nested("job")((DataflowConfig.descriptor.optional |@| boolean("supervise"))(JobConfig.apply, JobConfig.unapply))
  }

  def stringN(min: Int, max: Int): Gen[Random with Sized, String] =
    for {
      n <- Gen.int(min, max)
      s <- Gen.stringN(n)(alphaNumericChar)
    } yield s

}
