package zio.config.magnolia

import scala.deriving._
import scala.compiletime.{ erasedValue, constValue }

trait Fields[A] {
  def fields: List[String]
}

object Fields {
  inline given derived[T <: Tuple](using P: Mirror.ProductOf[A]): Fields[A] = new Fields[A] {
    override def fields: List[String] =
      inline erasedValue[T] match
        case _: EmptyTuple => Nil
        case _ : ( t *: ts) => constValue[t].toString :: derived[ts].fields
  }
}