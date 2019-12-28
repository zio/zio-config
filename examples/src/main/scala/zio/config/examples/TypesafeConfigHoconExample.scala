package zio.config.examples

import zio.config.typesafe.TypeSafeConfigSource._
import zio.config._, ConfigDescriptor._
import zio.DefaultRuntime
import zio.config.magnolia.ConfigDescriptorProvider._

object TypesafeConfigHoconExample extends App {
  val configSource =
    fromHoccon(Right("{ region : us-east, accountId: jon }"))

  final case class Aws(region: String, accountId: String)

  val configManual =
    ((string("region")) |@| string("accountId"))(Aws.apply, Aws.unapply)

  val runtime = new DefaultRuntime {}

  val result = runtime.unsafeRun(read(configManual from configSource))
  assert(result == Aws("us-east", "jon"))

  // A nested example with type safe config, and usage of magnolia
  final case class Account(region: String, accountId: String)
  final case class AwsConfig(account: Account)

  private val configAutomatic = description[AwsConfig]

  val configSourceNested =
    fromHoccon(Right("account { region : us-east, accountId: jon }"))

  val nestedConfigResult =
    runtime.unsafeRun(read(configAutomatic from configSourceNested))

  println(nestedConfigResult)

  assert(nestedConfigResult == AwsConfig(Account("us-east", "jon")))

}
