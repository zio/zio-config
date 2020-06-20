package zio.config.examples

import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor._

object OptionalExample extends App {
  case class AppConfig(jobName: String, details: Option[Detail])
  case class Detail(containerId: String, executionTime: String)

  val stringg =

    """
       jobName : hello
       details: {}
    """


  import zio.config.typesafe._


  val appConfigDesc =
    descriptor[AppConfig]

  val source: ConfigSource = TypesafeConfigSource.fromHoconString(stringg) match {
    case Right(a) => a
    case Left(_) => throw new Exception("bad hocon string")
  }
  
  assert(
    read(appConfigDesc from source)
  )
}