package zio.config

import scala.deriving._

trait TupleConversion[A, B] {
  def to(a: A): B
  def from(b: B): A
}

object TupleConversion extends ImplicitTupleConversion

trait ImplicitTupleConversion {
  given autoTupleConversion[Prod <: Product](using
    m: Mirror.ProductOf[Prod]
  ): TupleConversion[Prod, m.MirroredElemTypes] =
    new TupleConversion[Prod, m.MirroredElemTypes] {
      def to(a: Prod): m.MirroredElemTypes   = Tuple.fromProductTyped(a)
      def from(b: m.MirroredElemTypes): Prod = m.fromProduct(b)
    }

  inline given autoTupleConversion1[Prod <: Product, A](using
    c: TupleConversion[Prod, Tuple1[A]]
  ): TupleConversion[Prod, A] with {
    def to(a: Prod): A   = {
      val Tuple1(v) = c.to(a)
      v
    }
    def from(b: A): Prod = c.from(Tuple1(b))
  }
}
