package zio.config.examples.commandline

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config._

object NestedExample extends App {
  val cmdLineArgs =
    "--conf.key1 v1 --conf.key2 v2 --key3 v3"

  /**
   * One way to produce nested behaviour is to provide keyDelimiter as given below
   */
  val source =
    ConfigSource.fromCommandLineArgs(cmdLineArgs.split(' ').toList, keyDelimiter = Some('.'))

  final case class SparkConf(key1: String, key2: String)
  final case class A(conf: SparkConf, key3: String)

  assert(
    read(descriptor[A] from source) == Right(
      A(SparkConf("v1", "v2"), "v3")
    )
  )

  /**
   * Another way to produce nested behavior is not relying on keyDelimiter and instead form a commandLine Args as given below
   */
  val cmdLineArgsAlternative =
    "--conf -key1=v1 --conf -key2=v2 --key3 v3"

  val source2 =
    ConfigSource.fromCommandLineArgs(cmdLineArgsAlternative.split(' ').toList)

  assert(
    read(descriptor[A] from source2) == Right(
      A(SparkConf("v1", "v2"), "v3")
    )
  )

  /**
   *  A typical of using nesting is when we need to automatically derive config
   *  while keeping the configuration classes encapsulated and separated nicely.
   */
  final case class KinesisConfig(input: String, output: String)
  final case class S3Config(input: String, output: String)
  final case class AwsConfig(kinesis: KinesisConfig, s3: S3Config)
  final case class AppConfig(aws: AwsConfig, user: String)

  val awsCmdLineArgs =
    "--aws.kinesis.input=v1 --aws.kinesis.output=v2  --aws.s3.input=v3 --aws.s3.output=v4 --user jo"

  val source3 =
    ConfigSource.fromCommandLineArgs(awsCmdLineArgs.split(' ').toList, keyDelimiter = Some('.'))

  assert(
    read(descriptor[AppConfig] from source3) == Right(
      AppConfig(AwsConfig(KinesisConfig("v1", "v2"), S3Config("v3", "v4")), "jo")
    )
  )

  // The above example is equivalent to the below args which uses both type of nesting.
  // The alternative shows how flexible the implementation is, although you may choose to use the simplest approach
  val awsCmdLineArgs2 =
    "--aws -kinesis.input=v1 --aws -kinesis.output=v2  --aws -s3.input=v3 --aws -s3.output=v4 --user jo"
  // same as "--aws --kinesis.input=v1 --aws --kinesis.output=v2  --aws --s3.input=v3 --aws --s3.output=v4 --user jo"
  // same as "--aws --kinesis --input=v1 --aws --kinesis --output=v2  --aws --s3 --input=v3 --aws --s3 --output=v4 --user jo"
  // same as "--aws --kinesis --input v1 --aws --kinesis --output v2  --aws --s3 --input v3 --aws --s3 --output v4 --user jo"

  val source4 =
    ConfigSource.fromCommandLineArgs(awsCmdLineArgs2.split(' ').toList, keyDelimiter = Some('.'))

  assert(
    read(descriptor[AppConfig] from source4) == Right(
      AppConfig(AwsConfig(KinesisConfig("v1", "v2"), S3Config("v3", "v4")), "jo")
    )
  )
}
