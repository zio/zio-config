package zio.config

import ProductBuilder._

private[config] trait ProductBuilder[A, B] {
  val a: Config[A]
  val b: Config[B]

  def apply[C](f: (A, B) => C, g: C => Option[(A, B)]): Config[C] =
    a.zip(b).xmapEither { case (aa, bb) => Right(f(aa, bb)) }(liftWrite(g))

  def <*>[C](cc: Config[C]): ProductBuilder[C] =
    new ProductBuilder[C] {
      val c: Config[C] = cc
    }

  // /start/productbuilder/ (used by codegen - DO NOT REMOVE)

  sealed abstract class ProductBuilder[C] {
    val c: Config[C]
    def apply[D](ff: (A, B, C) => D, gg: D => Option[(A, B, C)]): Config[D] =
      (a zip b zip c).xmapEither[D] { case ((aa, bb), cc) => Right(ff(aa, bb, cc)) }(
        liftWrite(d => gg(d).map { case (aa, bb, cc) => ((aa, bb), cc) })
      )

    def <*>[D](dd: Config[D]): ProductBuilder[D] =
      new ProductBuilder[D] {
        val d: Config[D] = dd
      }

    sealed abstract class ProductBuilder[D] {
      val d: Config[D]
      def apply[E](ff: (A, B, C, D) => E, gg: E => Option[(A, B, C, D)]): Config[E] =
        (a zip b zip c zip d).xmapEither[E] { case (((aa, bb), cc), dd) => Right(ff(aa, bb, cc, dd)) }(
          liftWrite(e => gg(e).map { case (aa, bb, cc, dd) => (((aa, bb), cc), dd) })
        )

      def <*>[E](ee: Config[E]): ProductBuilder[E] =
        new ProductBuilder[E] {
          val e: Config[E] = ee
        }

      sealed abstract class ProductBuilder[E] {
        val e: Config[E]
        def apply[G](ff: (A, B, C, D, E) => G, gg: G => Option[(A, B, C, D, E)]): Config[G] =
          (a zip b zip c zip d zip e).xmapEither[G] { case ((((aa, bb), cc), dd), ee) => Right(ff(aa, bb, cc, dd, ee)) }(
            liftWrite(g => gg(g).map { case (aa, bb, cc, dd, ee) => ((((aa, bb), cc), dd), ee) })
          )

        def <*>[G](gg: Config[G]): ProductBuilder[G] =
          new ProductBuilder[G] {
            val g: Config[G] = gg
          }

        sealed abstract class ProductBuilder[G] {
          val g: Config[G]
          def apply[H](ff: (A, B, C, D, E, G) => H, gg: H => Option[(A, B, C, D, E, G)]): Config[H] =
            (a zip b zip c zip d zip e zip g).xmapEither[H] { case (((((aa, bb), cc), dd), ee), gg) => Right(ff(aa, bb, cc, dd, ee, gg)) }(
              liftWrite(h => gg(h).map { case (aa, bb, cc, dd, ee, gg) => (((((aa, bb), cc), dd), ee), gg) })
            )

          def <*>[H](hh: Config[H]): ProductBuilder[H] =
            new ProductBuilder[H] {
              val h: Config[H] = hh
            }

          sealed abstract class ProductBuilder[H] {
            val h: Config[H]
            def apply[I](ff: (A, B, C, D, E, G, H) => I, gg: I => Option[(A, B, C, D, E, G, H)]): Config[I] =
              (a zip b zip c zip d zip e zip g zip h).xmapEither[I] { case ((((((aa, bb), cc), dd), ee), gg), hh) => Right(ff(aa, bb, cc, dd, ee, gg, hh)) }(
                liftWrite(i => gg(i).map { case (aa, bb, cc, dd, ee, gg, hh) => ((((((aa, bb), cc), dd), ee), gg), hh) })
              )

            def <*>[I](ii: Config[I]): ProductBuilder[I] =
              new ProductBuilder[I] {
                val i: Config[I] = ii
              }

            sealed abstract class ProductBuilder[I] {
              val i: Config[I]
              def apply[J](ff: (A, B, C, D, E, G, H, I) => J, gg: J => Option[(A, B, C, D, E, G, H, I)]): Config[J] =
                (a zip b zip c zip d zip e zip g zip h zip i).xmapEither[J] { case (((((((aa, bb), cc), dd), ee), gg), hh), ii) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii)) }(
                  liftWrite(j => gg(j).map { case (aa, bb, cc, dd, ee, gg, hh, ii) => (((((((aa, bb), cc), dd), ee), gg), hh), ii) })
                )

              def <*>[J](jj: Config[J]): ProductBuilder[J] =
                new ProductBuilder[J] {
                  val j: Config[J] = jj
                }

              sealed abstract class ProductBuilder[J] {
                val j: Config[J]
                def apply[K](ff: (A, B, C, D, E, G, H, I, J) => K, gg: K => Option[(A, B, C, D, E, G, H, I, J)]): Config[K] =
                  (a zip b zip c zip d zip e zip g zip h zip i zip j).xmapEither[K] { case ((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj)) }(
                    liftWrite(k => gg(k).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj) => ((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj) })
                  )

                def <*>[K](kk: Config[K]): ProductBuilder[K] =
                  new ProductBuilder[K] {
                    val k: Config[K] = kk
                  }

                sealed abstract class ProductBuilder[K] {
                  val k: Config[K]
                  def apply[L](ff: (A, B, C, D, E, G, H, I, J, K) => L, gg: L => Option[(A, B, C, D, E, G, H, I, J, K)]): Config[L] =
                    (a zip b zip c zip d zip e zip g zip h zip i zip j zip k).xmapEither[L] { case (((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk)) }(
                      liftWrite(l => gg(l).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk) => (((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk) })
                    )

                  def <*>[L](ll: Config[L]): ProductBuilder[L] =
                    new ProductBuilder[L] {
                      val l: Config[L] = ll
                    }

                  sealed abstract class ProductBuilder[L] {
                    val l: Config[L]
                    def apply[M](ff: (A, B, C, D, E, G, H, I, J, K, L) => M, gg: M => Option[(A, B, C, D, E, G, H, I, J, K, L)]): Config[M] =
                      (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l).xmapEither[M] { case ((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll)) }(
                        liftWrite(m => gg(m).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll) => ((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll) })
                      )

                    def <*>[M](mm: Config[M]): ProductBuilder[M] =
                      new ProductBuilder[M] {
                        val m: Config[M] = mm
                      }

                    sealed abstract class ProductBuilder[M] {
                      val m: Config[M]
                      def apply[N](ff: (A, B, C, D, E, G, H, I, J, K, L, M) => N, gg: N => Option[(A, B, C, D, E, G, H, I, J, K, L, M)]): Config[N] =
                        (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m).xmapEither[N] { case (((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm)) }(
                          liftWrite(n => gg(n).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm) => (((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm) })
                        )

                      def <*>[N](nn: Config[N]): ProductBuilder[N] =
                        new ProductBuilder[N] {
                          val n: Config[N] = nn
                        }

                      sealed abstract class ProductBuilder[N] {
                        val n: Config[N]
                        def apply[O](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N) => O, gg: O => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N)]): Config[O] =
                          (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n).xmapEither[O] { case ((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn)) }(
                            liftWrite(o => gg(o).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn) => ((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn) })
                          )

                        def <*>[O](oo: Config[O]): ProductBuilder[O] =
                          new ProductBuilder[O] {
                            val o: Config[O] = oo
                          }

                        sealed abstract class ProductBuilder[O] {
                          val o: Config[O]
                          def apply[P](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O) => P, gg: P => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O)]): Config[P] =
                            (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o).xmapEither[P] { case (((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo)) }(
                              liftWrite(p => gg(p).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo) => (((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo) })
                            )

                          def <*>[P](pp: Config[P]): ProductBuilder[P] =
                            new ProductBuilder[P] {
                              val p: Config[P] = pp
                            }

                          sealed abstract class ProductBuilder[P] {
                            val p: Config[P]
                            def apply[Q](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P) => Q, gg: Q => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P)]): Config[Q] =
                              (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p).xmapEither[Q] { case ((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp)) }(
                                liftWrite(q => gg(q).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp) => ((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp) })
                              )

                            def <*>[Q](qq: Config[Q]): ProductBuilder[Q] =
                              new ProductBuilder[Q] {
                                val q: Config[Q] = qq
                              }

                            sealed abstract class ProductBuilder[Q] {
                              val q: Config[Q]
                              def apply[R](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q) => R, gg: R => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q)]): Config[R] =
                                (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q).xmapEither[R] { case (((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq)) }(
                                  liftWrite(r => gg(r).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq) => (((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq) })
                                )

                              def <*>[R](rr: Config[R]): ProductBuilder[R] =
                                new ProductBuilder[R] {
                                  val r: Config[R] = rr
                                }

                              sealed abstract class ProductBuilder[R] {
                                val r: Config[R]
                                def apply[S](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R) => S, gg: S => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R)]): Config[S] =
                                  (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r).xmapEither[S] { case ((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr)) }(
                                    liftWrite(s => gg(s).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr) => ((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr) })
                                  )

                                def <*>[S](ss: Config[S]): ProductBuilder[S] =
                                  new ProductBuilder[S] {
                                    val s: Config[S] = ss
                                  }

                                sealed abstract class ProductBuilder[S] {
                                  val s: Config[S]
                                  def apply[T](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T, gg: T => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S)]): Config[T] =
                                    (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s).xmapEither[T] { case (((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss)) }(
                                      liftWrite(t => gg(t).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss) => (((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss) })
                                    )

                                  def <*>[T](tt: Config[T]): ProductBuilder[T] =
                                    new ProductBuilder[T] {
                                      val t: Config[T] = tt
                                    }

                                  sealed abstract class ProductBuilder[T] {
                                    val t: Config[T]
                                    def apply[U](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U, gg: U => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]): Config[U] =
                                      (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t).xmapEither[U] { case ((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt)) }(
                                        liftWrite(u => gg(u).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt) => ((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt) })
                                      )

                                    def <*>[U](uu: Config[U]): ProductBuilder[U] =
                                      new ProductBuilder[U] {
                                        val u: Config[U] = uu
                                      }

                                    sealed abstract class ProductBuilder[U] {
                                      val u: Config[U]
                                      def apply[V](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V, gg: V => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]): Config[V] =
                                        (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t zip u).xmapEither[V] { case (((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt), uu) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt, uu)) }(
                                          liftWrite(v => gg(v).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt, uu) => (((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt), uu) })
                                        )

                                      def <*>[V](vv: Config[V]): ProductBuilder[V] =
                                        new ProductBuilder[V] {
                                          val v: Config[V] = vv
                                        }

                                      sealed abstract class ProductBuilder[V] {
                                        val v: Config[V]
                                        def apply[W](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W, gg: W => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]): Config[W] =
                                          (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t zip u zip v).xmapEither[W] { case ((((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt), uu), vv) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt, uu, vv)) }(
                                            liftWrite(w => gg(w).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt, uu, vv) => ((((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt), uu), vv) })
                                          )

                                        def <*>[W](ww: Config[W]): ProductBuilder[W] =
                                          new ProductBuilder[W] {
                                            val w: Config[W] = ww
                                          }

                                        sealed abstract class ProductBuilder[W] {
                                          val w: Config[W]
                                          def apply[X](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) => X, gg: X => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W)]): Config[X] =
                                            (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t zip u zip v zip w).xmapEither[X] { case (((((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt), uu), vv), ww) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt, uu, vv, ww)) }(
                                              liftWrite(x => gg(x).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt, uu, vv, ww) => (((((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt), uu), vv), ww) })
                                            )
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  // /end/productbuilder/ (used by codegen - DO NOT REMOVE)
}

private[config] object ProductBuilder {
  private def liftWrite[A, B, C](f: C => Option[(A, B)]): C => Either[String, (A, B)] =
    c => f(c).fold[Either[String, (A, B)]](Left("Failed to write the value back."))(r => Right(r))

}
