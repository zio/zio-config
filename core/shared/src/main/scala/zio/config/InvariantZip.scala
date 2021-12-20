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

  implicit def invariantZipTuple7[A, B, C, D, E, F, G, Z]
    : InvariantZip.WithOut[(A, B, C, D, E, F, G), Z, (A, B, C, D, E, F, G, Z)] =
    InvariantZip
      .instance[(A, B, C, D, E, F, G), Z, (A, B, C, D, E, F, G, Z)]({ case ((a, b, c, d, e, f, g), z) =>
        (a, b, c, d, e, f, g, z)
      })(
        z => (z._1, z._2, z._3, z._4, z._5, z._6, z._7),
        z => z._8
      )

  implicit def invariantZipTuple8[A, B, C, D, E, F, G, H, Z]
    : InvariantZip.WithOut[(A, B, C, D, E, F, G, H), Z, (A, B, C, D, E, F, G, H, Z)] =
    InvariantZip
      .instance[(A, B, C, D, E, F, G, H), Z, (A, B, C, D, E, F, G, H, Z)]({ case ((a, b, c, d, e, f, g, h), z) =>
        (a, b, c, d, e, f, g, h, z)
      })(
        z => (z._1, z._2, z._3, z._4, z._5, z._6, z._7, z._8),
        z => z._9
      )

  implicit def invariantZipTuple9[A, B, C, D, E, F, G, H, I, Z]
    : InvariantZip.WithOut[(A, B, C, D, E, F, G, H, I), Z, (A, B, C, D, E, F, G, H, I, Z)] =
    InvariantZip
      .instance[(A, B, C, D, E, F, G, H, I), Z, (A, B, C, D, E, F, G, H, I, Z)]({
        case ((a, b, c, d, e, f, g, h, i), z) =>
          (a, b, c, d, e, f, g, h, i, z)
      })(
        z => (z._1, z._2, z._3, z._4, z._5, z._6, z._7, z._8, z._9),
        z => z._10
      )

  implicit def invariantZipTuple10[A, B, C, D, E, F, G, H, I, J, Z]
    : InvariantZip.WithOut[(A, B, C, D, E, F, G, H, I, J), Z, (A, B, C, D, E, F, G, H, I, J, Z)] =
    InvariantZip
      .instance[(A, B, C, D, E, F, G, H, I, J), Z, (A, B, C, D, E, F, G, H, I, J, Z)]({
        case ((a, b, c, d, e, f, g, h, i, j), z) =>
          (a, b, c, d, e, f, g, h, i, j, z)
      })(
        z => (z._1, z._2, z._3, z._4, z._5, z._6, z._7, z._8, z._9, z._10),
        z => z._11
      )

  implicit def invariantZipTuple11[A, B, C, D, E, F, G, H, I, J, K, Z]
    : InvariantZip.WithOut[(A, B, C, D, E, F, G, H, I, J, K), Z, (A, B, C, D, E, F, G, H, I, J, K, Z)] =
    InvariantZip
      .instance[(A, B, C, D, E, F, G, H, I, J, K), Z, (A, B, C, D, E, F, G, H, I, J, K, Z)]({
        case ((a, b, c, d, e, f, g, h, i, j, k), z) =>
          (a, b, c, d, e, f, g, h, i, j, k, z)
      })(
        z => (z._1, z._2, z._3, z._4, z._5, z._6, z._7, z._8, z._9, z._10, z._11),
        z => z._12
      )

  implicit def invariantZipTuple12[A, B, C, D, E, F, G, H, I, J, K, L, Z]
    : InvariantZip.WithOut[(A, B, C, D, E, F, G, H, I, J, K, L), Z, (A, B, C, D, E, F, G, H, I, J, K, L, Z)] =
    InvariantZip
      .instance[(A, B, C, D, E, F, G, H, I, J, K, L), Z, (A, B, C, D, E, F, G, H, I, J, K, L, Z)]({
        case ((a, b, c, d, e, f, g, h, i, j, k, l), z) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, z)
      })(
        z => (z._1, z._2, z._3, z._4, z._5, z._6, z._7, z._8, z._9, z._10, z._11, z._12),
        z => z._13
      )

  implicit def invariantZipTuple13[A, B, C, D, E, F, G, H, I, J, K, L, M, Z]
    : InvariantZip.WithOut[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)] =
    InvariantZip
      .instance[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)]({
        case ((a, b, c, d, e, f, g, h, i, j, k, l, m), z) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, z)
      })(
        z => (z._1, z._2, z._3, z._4, z._5, z._6, z._7, z._8, z._9, z._10, z._11, z._12, z._13),
        z => z._14
      )

  implicit def invariantZipTuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z]: InvariantZip.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)
  ] =
    InvariantZip
      .instance[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)]({
        case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n), z) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, z)
      })(
        z => (z._1, z._2, z._3, z._4, z._5, z._6, z._7, z._8, z._9, z._10, z._11, z._12, z._13, z._14),
        z => z._15
      )

  implicit def invariantZipTuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z]: InvariantZip.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)
  ] =
    InvariantZip
      .instance[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)]({
        case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o), z) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, z)
      })(
        z => (z._1, z._2, z._3, z._4, z._5, z._6, z._7, z._8, z._9, z._10, z._11, z._12, z._13, z._14, z._15),
        z => z._16
      )

  implicit def invariantZipTuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z]: InvariantZip.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)
  ] =
    InvariantZip
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)
      ]({ case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, z)
      })(
        z => (z._1, z._2, z._3, z._4, z._5, z._6, z._7, z._8, z._9, z._10, z._11, z._12, z._13, z._14, z._15, z._16),
        z => z._17
      )

  implicit def invariantZipTuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z]: InvariantZip.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)
  ] =
    InvariantZip
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)
      ]({ case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, z)
      })(
        z =>
          (
            z._1,
            z._2,
            z._3,
            z._4,
            z._5,
            z._6,
            z._7,
            z._8,
            z._9,
            z._10,
            z._11,
            z._12,
            z._13,
            z._14,
            z._15,
            z._16,
            z._17
          ),
        z => z._18
      )

  implicit def invariantZipTuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z]: InvariantZip.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)
  ] =
    InvariantZip
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)
      ]({ case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, z)
      })(
        z =>
          (
            z._1,
            z._2,
            z._3,
            z._4,
            z._5,
            z._6,
            z._7,
            z._8,
            z._9,
            z._10,
            z._11,
            z._12,
            z._13,
            z._14,
            z._15,
            z._16,
            z._17,
            z._18
          ),
        z => z._19
      )

  implicit def invariantZipTuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z]: InvariantZip.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)
  ] =
    InvariantZip
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)
      ]({ case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, z)
      })(
        z =>
          (
            z._1,
            z._2,
            z._3,
            z._4,
            z._5,
            z._6,
            z._7,
            z._8,
            z._9,
            z._10,
            z._11,
            z._12,
            z._13,
            z._14,
            z._15,
            z._16,
            z._17,
            z._18,
            z._19
          ),
        z => z._20
      )

  implicit def invariantZipTuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z]: InvariantZip.WithOut[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)
  ] =
    InvariantZip
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)
      ]({ case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, z)
      })(
        z =>
          (
            z._1,
            z._2,
            z._3,
            z._4,
            z._5,
            z._6,
            z._7,
            z._8,
            z._9,
            z._10,
            z._11,
            z._12,
            z._13,
            z._14,
            z._15,
            z._16,
            z._17,
            z._18,
            z._19,
            z._20
          ),
        z => z._21
      )

  implicit def invariantZipTuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z]
    : InvariantZip.WithOut[
      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
      Z,
      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)
    ] =
    InvariantZip
      .instance[
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
        Z,
        (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)
      ]({ case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u), z) =>
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, z)
      })(
        z =>
          (
            z._1,
            z._2,
            z._3,
            z._4,
            z._5,
            z._6,
            z._7,
            z._8,
            z._9,
            z._10,
            z._11,
            z._12,
            z._13,
            z._14,
            z._15,
            z._16,
            z._17,
            z._18,
            z._19,
            z._20,
            z._21
          ),
        z => z._22
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
