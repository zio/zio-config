package zio.config.examples.commandline

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.ConfigSource
import zio.config.string._

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
}
