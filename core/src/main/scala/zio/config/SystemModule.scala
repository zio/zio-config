package zio.config

import java.util.Properties

import zio._

object SystemModule {
  type SystemModule = Has[SystemModule.Service]

  trait Service {
    def getEnvironment: Task[Map[String, String]]
    def getProperties: Task[Properties]
  }

  def live: ULayer[SystemModule] =
    ZLayer.succeed(new Service {
      override def getEnvironment: Task[Map[String, String]] = Task.effect(sys.env)
      override def getProperties: Task[Properties]           = Task.effect(java.lang.System.getProperties)
    })

  def test(envMap: Map[String, String] = Map.empty, propMap: Map[String, String] = Map.empty): ULayer[Config[Service]] =
    ZLayer.succeed(new Service {
      override def getEnvironment: Task[Map[String, String]] = Task.effectTotal(envMap)
      override def getProperties: Task[Properties] = Task.effectTotal {
        val prop = new Properties
        propMap.map {
          case (key, value) => prop.setProperty(key, value)
        }
        prop
      }
    })

  def getEnvironment: ZIO[SystemModule, Throwable, Map[String, String]] =
    ZIO.accessM(_.get.getEnvironment)

}
