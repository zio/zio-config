package zio.config.examples

import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor._
import zio.config.typesafe._

object OptionalExample extends App {

  case class AppConfig(jobName: String, hello: Option[Detail])

  case class Detail(x: String, y: Either[Int, String])

  val validConfig =
    """
       jobName: 1
       hello: {
         x : 1
         y: 1
       }
    """

  def getSource(str: String): ConfigSource = TypesafeConfigSource.fromHoconString(str) match {
    case Right(a) => a
    case Left(_)  => throw new Exception("bad hocon string")
  }

  assert(read(descriptor[AppConfig] from getSource(validConfig)) == Right(AppConfig("1", Some(Detail("1", Left(1))))))

  val validConfig2 =
    """
       jobName: 1
       hello: null
    """

  assert(read(descriptor[AppConfig] from getSource(validConfig2)) == Right(AppConfig("1", None)))

  val validConfig3 =
    """
       jobName: 1
    """

  assert(read(descriptor[AppConfig] from getSource(validConfig3)) == Right(AppConfig("1", None)))

  val invalidConfig =
    """
       jobName: 1
       hello: {
         x : 1
       }
    """

  assert(read(descriptor[AppConfig] from getSource(invalidConfig)).isLeft)
    /*
     ╥
     ╠─MissingValue
     ║ path: hello.y
     ║ Details: optional value, value of type int
     ║
     ╠─MissingValue
     ║ path: hello.y
     ║ Details: optional value, value of type string
     ▼
     */
}
