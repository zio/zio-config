package zio.config.aws.parameterstore

import zio.{Has, Task}
import zio.config.{PropertyTree, PropertyTreePath}

import scala.language.higherKinds

object parameterstore {
  type Service = Has[ParameterStore.Service]

  object ParameterStore {
    trait Service {
      def getValue(path: PropertyTreePath[String]): Task[PropertyTree[String, String]]
    }
  }
}
