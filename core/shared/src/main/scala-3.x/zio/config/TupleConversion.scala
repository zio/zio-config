package zio.config

import scala.deriving._

/**
 * Converts between a case class `A` and `B`, the tuple of its field types. `Config#to` uses it to turn a config of a
 * tuple into a config of a case class. `ImplicitTupleConversion` derives instances for case classes: on Scala 2 with a
 * macro, on Scala 3 from the case class's `Mirror`.
 *
 * {{{
 * import zio.config._
 *
 * final case class Endpoint(host: String, port: Int)
 *
 * val conversion = implicitly[TupleConversion[Endpoint, (String, Int)]]
 *
 * val fields: (String, Int) = conversion.to(Endpoint("api.example.com", 443))
 * // (api.example.com,443)
 * }}}
 */
trait TupleConversion[A, B] {

  /**
   * Returns the fields of `a` as a `B`.
   */
  def to(a: A): B

  /**
   * Builds an `A` from the fields in `b`.
   */
  def from(b: B): A
}

/**
 * Companion of [[TupleConversion]].
 */
object TupleConversion extends ImplicitTupleConversion

/**
 * Provides the `TupleConversion` instances for case classes. The `zio.config` package object extends it, so
 * `import zio.config._` brings them into scope.
 */
trait ImplicitTupleConversion {

  /**
   * Derives a `TupleConversion` from a case class to the tuple of its field types. On Scala 2, a case class with one
   * field converts to that field's type. On Scala 3, it converts to a `Tuple1`, and `autoTupleConversion1` gives the
   * conversion to the field's type.
   */
  given autoTupleConversion[Prod <: Product](using
    m: Mirror.ProductOf[Prod]
  ): TupleConversion[Prod, m.MirroredElemTypes] =
    new TupleConversion[Prod, m.MirroredElemTypes] {
      def to(a: Prod): m.MirroredElemTypes   = Tuple.fromProductTyped(a)
      def from(b: m.MirroredElemTypes): Prod = m.fromProduct(b)
    }

  /**
   * On Scala 3, turns a conversion to `Tuple1[A]` into a conversion to `A`, so a case class with one field converts to
   * that field's type. On Scala 2 it is not implicit and has no implementation: it exists so that code built for both
   * versions can import it by name, and calling it throws `scala.NotImplementedError`.
   */
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
