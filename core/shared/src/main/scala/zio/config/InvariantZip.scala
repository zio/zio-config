package zio.config

trait InvariantZip[A, B] {
  type Out

  def combine(a: A, b: B): Out

  def projectLeft(o: Out): A
  def projectRight(o: Out): B
}

object InvariantZip extends InvariantZipLowPriority0 {
  type WithOut[A, B, C] = InvariantZip[A, B] { type Out = C }

  def instance[A, B, C](f: (A, B) => C)(left: C => A, right: C => B): InvariantZip.WithOut[A, B, C] =
    new InvariantZip[A, B] {
      type Out = C

      def combine(a: A, b: B): C  = f(a, b)
      def projectLeft(o: Out): A  = left(o)
      def projectRight(o: Out): B = right(o)
    }

  implicit def invariantZipTuple2Unit[A, B]: InvariantZip.WithOut[(A, B), Unit, (A, B)] =
    instance[(A, B), Unit, (A, B)]((tuple, _) => tuple)(identity, _ => ())

  implicit def invariantZipUnitB[B]: WithOut[Unit, B, B] = new InvariantZip[Unit, B] {
    type Out = B

    override def combine(a: Unit, b: B): Out = b

    override def projectLeft(o: B): Unit = ()

    override def projectRight(o: B): B = o
  }

  implicit def invariantZipAUnit[A]: WithOut[A, Unit, A] = new InvariantZip[A, Unit] {
    type Out = A

    override def combine(a: A, b: Unit): Out = a

    override def projectLeft(o: A): A = o

    override def projectRight(o: A): Unit = ()
  }
}

trait InvariantZipLowPriority0 extends InvariantZipLowPriority1 {
  implicit def invariantZipTuple2[A, B, Z]: InvariantZip.WithOut[(A, B), Z, (A, B, Z)]                   =
    InvariantZip.instance[(A, B), Z, (A, B, Z)]({ case ((a, b), z) => (a, b, z) })(z => (z._1, z._2), z => z._3)

  implicit def invariantZipTuple3[A, B, C, Z]: InvariantZip.WithOut[(A, B, C), Z, (A, B, C, Z)]          =
    InvariantZip
      .instance[(A, B, C), Z, (A, B, C, Z)]({ case ((a, b, c), z) => (a, b, c, z) })(z => (z._1, z._2, z._3), z => z._4)

  implicit def invariantZipTuple4[A, B, C, D, Z]: InvariantZip.WithOut[(A, B, C, D), Z, (A, B, C, D, Z)] =
    InvariantZip
      .instance[(A, B, C, D), Z, (A, B, C, D, Z)]({ case ((a, b, c, d), z) => (a, b, c, d, z) })(
        z => (z._1, z._2, z._3, z._4),
        z => z._5
      )

  implicit def invariantZipTuple5[A, B, C, D, E, Z]: InvariantZip.WithOut[(A, B, C, D, E), Z, (A, B, C, D, E, Z)] =
    InvariantZip
      .instance[(A, B, C, D, E), Z, (A, B, C, D, E, Z)]({ case ((a, b, c, d, e), z) => (a, b, c, d, e, z) })(
        z => (z._1, z._2, z._3, z._4, z._5),
        z => z._6
      )

  implicit def invariantZipTuple6[A, B, C, D, E, F, Z]
    : InvariantZip.WithOut[(A, B, C, D, E, F), Z, (A, B, C, D, E, F, Z)] =
    InvariantZip
      .instance[(A, B, C, D, E, F), Z, (A, B, C, D, E, F, Z)]({ case ((a, b, c, d, e, f), z) =>
        (a, b, c, d, e, f, z)
      })(
        z => (z._1, z._2, z._3, z._4, z._5, z._6),
        z => z._7
      )

}

trait InvariantZipLowPriority1 {
  implicit def invariantZipAB[A, B]: InvariantZip.WithOut[A, B, (A, B)] = new InvariantZip[A, B] {
    type Out = (A, B)

    override def combine(a: A, b: B): (A, B) = ((a, b))

    override def projectLeft(o: Out): A = o._1

    override def projectRight(o: Out): B = o._2
  }
}
