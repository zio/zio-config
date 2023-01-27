package zio.config.examples.commandline

import zio.config._
import zio.config.magnolia.descriptor

import examples._
import Config._

object CommandLineSimple extends App {
  val cmdLineArgs =
    "--key1 value1 --key2 value2"

  val source: ConfigSource =
    ConfigSource.fromCommandLineArgs(cmdLineArgs.split(' ').toList)

  final case class A(key1: String, key2: String)

  assert(
    read(descriptor[A] from ConfigSource.fromCommandLineArgs(cmdLineArgs.split(' ').toList)) equalM
      A("value1", "value2")
  )

  assert(
    read((string("key1") zip string("key2")).to[A] from source) equalM
      A("value1", "value2")
  )

  /**
   * Keys and Values could be also separated with '='
   */
  val cmdLineArgsWithEqualSeparator =
    "--key1=value1 --key2=value2"

  val source2: ConfigSource = ConfigSource.fromCommandLineArgs(cmdLineArgsWithEqualSeparator.split(' ').toList)

  assert(
    read(descriptor[A] from source2) equalM A("value1", "value2")
  )

  /**
   * Values could contain '=' but only when key and value are separated also by equal sign.
   */
  val cmdLineArgsWithEqualInValue =
    "--key1==1 --key2=value="

  val source3: ConfigSource = ConfigSource.fromCommandLineArgs(cmdLineArgsWithEqualInValue.split(' ').toList)

  assert {
    read(descriptor[A] from source3) equalM
      A("=1", "value=")
  }
}
