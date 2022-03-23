package zio.config.examples.magnolia

import zio.config._, ConfigDescriptor._, magnolia._, typesafe._

object SealedTrait extends App {
  sealed trait Y

  object Y {
    case class A(age: Int)     extends Y
    case class B(name: String) extends Y
  }

  case class AppConfig(x: Y)

  val str =
    s"""
             x : {
               age : 10
             }
            """

  zio.Runtime.default.unsafeRun(
    read(descriptorWithoutClassNames[AppConfig] from ConfigSource.fromHoconString(str))
  )

}
