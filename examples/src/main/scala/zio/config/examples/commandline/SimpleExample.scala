package zio.config.examples.commandline

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.ConfigSource
import zio.config.read
import zio.config.ConfigDescriptor, ConfigDescriptor._

object SimpleExample extends App {
  val cmdLineArgs =
    "--key1 value1 --key2 value2"

  val source =
    ConfigSource.fromCommandLineArgs(cmdLineArgs.split(' ').toList)

  final case class A(key1: String, key2: String)

  assert(
    read(descriptor[A] from ConfigSource.fromCommandLineArgs(cmdLineArgs.split(' ').toList)) == Right(
      A("value1", "value2")
    )
  )

  assert(
    read((string("key1") |@| string("key2"))(A.apply, A.unapply) from source) == Right(A("value1", "value2"))
  )

  /**
   * Keys and Values could be also separated with '='
   */
  val cmdLineArgsWithEqualSeparator =
    "--key1=value1 --key2=value2"

  val source2 = ConfigSource.fromCommandLineArgs(cmdLineArgsWithEqualSeparator.split(' ').toList)

  assert(
    read(descriptor[A] from source2) == Right(A("value1", "value2"))
  )

  /**
   * Values could contain '=' but only when key and value are separated also by equal sign.
   */
  val cmdLineArgsWithEqualInValue =
    "--key1==1 --key2=value="

  val source3 = ConfigSource.fromCommandLineArgs(cmdLineArgsWithEqualInValue.split(' ').toList)

  assert {
    read(descriptor[A] from source3) == Right(
      A("=1", "value=")
    )
  }
}
