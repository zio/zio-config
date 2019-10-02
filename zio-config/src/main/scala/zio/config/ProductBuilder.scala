package zio.config

import ProductBuilder._

private[config] trait ProductBuilder[A, B] {
  val a: ConfigDescriptor[A]
  val b: ConfigDescriptor[B]

  def apply[C](f: (A, B) => C, g: C => Option[(A, B)]): ConfigDescriptor[C] =
    a.zip(b).xmapEither({ case (aa, bb) => Right(f(aa, bb)) })(liftWrite(g))

  def <*>[C](cc: ConfigDescriptor[C]): ProductBuilder[C] = new ProductBuilder[C] {
    val c: ConfigDescriptor[C] = cc
  }

  sealed abstract class ProductBuilder[C] {
    val c: ConfigDescriptor[C]

    def apply[D](f: (A, B, C) => D, g: D => Option[(A, B, C)]): ConfigDescriptor[D] =
      (a zip b zip c)
        .xmapEither[D]({ case ((aa, bb), cc) => Right(f(aa, bb, cc)) })(
          liftWrite(d => g(d).map({ case (aa, bb, cc) => ((aa, bb), cc) }))
        )

    def <*>[D](dd: ConfigDescriptor[D]): ProductBuilder[D] = new ProductBuilder[D] {
      val d: ConfigDescriptor[D] = dd
    }

    sealed abstract class ProductBuilder[D] {
      val d: ConfigDescriptor[D]

      def apply[E](f: (A, B, C, D) => E, g: E => Option[(A, B, C, D)]): ConfigDescriptor[E] =
        (a zip b zip c zip d)
          .xmapEither(
            { case (((aa, bb), cc), dd) => Right(f(aa, bb, cc, dd)) }
          )(
            liftWrite(e => g(e).map { case (aa, bb, cc, dd) => (((aa, bb), cc), dd) })
          )

      def <*>[E](ee: ConfigDescriptor[E]): ProductBuilder[E] = new ProductBuilder[E] {
        val e: ConfigDescriptor[E] = ee
      }

      sealed abstract class ProductBuilder[E] {
        val e: ConfigDescriptor[E]

        def apply[FF](f: (A, B, C, D, E) => FF, g: FF => Option[(A, B, C, D, E)]): ConfigDescriptor[FF] =
          (a zip b zip c zip d zip e)
            .xmapEither(
              { case ((((aa, bb), cc), dd), ee) => Right(f(aa, bb, cc, dd, ee)) }
            )(
              liftWrite(ff => g(ff).map({ case (aa, bb, cc, dd, ee) => ((((aa, bb), cc), dd), ee) }))
            )

        def <*>[FF](dd: ConfigDescriptor[FF]): ProductBuilder[FF] = new ProductBuilder[FF] {
          val ff: ConfigDescriptor[FF] = dd
        }

        sealed abstract class ProductBuilder[FF] {
          val ff: ConfigDescriptor[FF]

          def apply[G](f: (A, B, C, D, E, FF) => G, g: G => Option[(A, B, C, D, E, FF)]): ConfigDescriptor[G] =
            (a zip b zip c zip d zip e zip ff)
              .xmapEither(
                { case (((((aa, bb), cc), dd), ee), fff) => Right(f(aa, bb, cc, dd, ee, fff)) }
              )(
                liftWrite(g(_).map({ case (aa, bb, cc, dd, ee, fff) => (((((aa, bb), cc), dd), ee), fff) }))
              )

          def <*>[G](dd: ConfigDescriptor[G]): ProductBuilder[G] = new ProductBuilder[G] {
            val g: ConfigDescriptor[G] = dd
          }

          sealed abstract class ProductBuilder[G] {
            val g: ConfigDescriptor[G]

            def apply[H](f: (A, B, C, D, E, FF, G) => H, gg: H => Option[(A, B, C, D, E, FF, G)]): ConfigDescriptor[H] =
              (a zip b zip c zip d zip e zip ff zip g)
                .xmapEither(
                  { case ((((((aa, bb), cc), dd), ee), fff), ggg) => Right(f(aa, bb, cc, dd, ee, fff, ggg)) }
                )(
                  liftWrite(gg(_).map({
                    case (aa, bb, cc, dd, ee, fff, ggg) => ((((((aa, bb), cc), dd), ee), fff), ggg)
                  }))
                )

            def <*>[H](dd: ConfigDescriptor[H]): ProductBuilder[H] = new ProductBuilder[H] {
              val h: ConfigDescriptor[H] = dd
            }

            sealed abstract class ProductBuilder[H] {
              val h: ConfigDescriptor[H]

              def apply[I](
                f: (A, B, C, D, E, FF, G, H) => I,
                gg: I => Option[(A, B, C, D, E, FF, G, H)]
              ): ConfigDescriptor[I] =
                (a zip b zip c zip d zip e zip ff zip g zip h)
                  .xmapEither(
                    {
                      case (((((((aa, bb), cc), dd), ee), fff), ggg), hh) => Right(f(aa, bb, cc, dd, ee, fff, ggg, hh))
                    }
                  )(
                    liftWrite(gg(_).map {
                      case (aa, bb, cc, dd, ee, fff, ggg, hh) => (((((((aa, bb), cc), dd), ee), fff), ggg), hh)
                    })
                  )

              def <*>[I](dd: ConfigDescriptor[I]): ProductBuilder[I] = new ProductBuilder[I] {
                val i: ConfigDescriptor[I] = dd
              }

              sealed abstract class ProductBuilder[I] {
                val i: ConfigDescriptor[I]

                def apply[J](
                  f: (A, B, C, D, E, FF, G, H, I) => J,
                  gg: J => Option[(A, B, C, D, E, FF, G, H, I)]
                ): ConfigDescriptor[J] =
                  (a zip b zip c zip d zip e zip ff zip g zip h zip i)
                    .xmapEither(
                      {
                        case ((((((((aa, bb), cc), dd), ee), fff), ggg), hh), ii) =>
                          Right(f(aa, bb, cc, dd, ee, fff, ggg, hh, ii))
                      }
                    )(liftWrite(gg(_).map({
                      case (aa, bb, cc, dd, ee, fff, ggg, hh, ii) =>
                        ((((((((aa, bb), cc), dd), ee), fff), ggg), hh), ii)
                    })))
              }

            }

          }

        }

      }

    }

  }
}

private[config] object ProductBuilder {
  private def liftWrite[A, B, C](f: C => Option[(A, B)]): C => Either[String, (A, B)] =
    c => f(c).fold[Either[String, (A, B)]](Left("Failed to write the value back."))(r => Right(r))

}
