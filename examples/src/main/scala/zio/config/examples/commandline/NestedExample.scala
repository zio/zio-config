package zio.config.examples.commandline

import zio.config.ConfigSource
import zio.config.magnolia.DeriveConfigDescriptor
import zio.config._, ConfigDescriptor._

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
    read(DeriveConfigDescriptor[A] from source) == Right(
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
    read(DeriveConfigDescriptor[A] from source2) == Right(
      A(SparkConf("v1", "v2"), "v3")
    )
  )
}
