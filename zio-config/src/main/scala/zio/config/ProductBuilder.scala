package zio.config

import ProductBuilder._

private[config] trait ProductBuilder[KK, VV, A, B] {
  val a: ConfigDescriptor[KK, VV, A]
  val b: ConfigDescriptor[KK, VV, B]

  def apply[C](f: (A, B) => C, g: C => Option[(A, B)]): ConfigDescriptor[KK, VV, C] =
    a.zip(b).xmapEither({ case (aa, bb) => Right(f(aa, bb)) })(liftWrite(g))

  def |@|[C](cc: ConfigDescriptor[KK, VV, C]): ProductBuilder[KK, VV, C] = new ProductBuilder[KK, VV, C] {
    val c: ConfigDescriptor[KK, VV, C] = cc
  }

  // /start/productbuilder/ (used by codegen - DO NOT REMOVE)

  sealed abstract class ProductBuilder[C] {
    val c: ConfigDescriptor[KK, VV, C]
    def apply[D](ff: (A, B, C) => D, gg: D => Option[(A, B, C)]): ConfigDescriptor[KK, VV, D] =
      (a zip b zip c)
        .xmapEither[KK, VV, D] {
          case ((aa, bb), cc) => Right(ff(aa, bb, cc))
        }(
          liftWrite(d => gg(d).map { case (aa, bb, cc) => ((aa, bb), cc) })
        )

    def |@|[D](dd: ConfigDescriptor[KK, VV, D]): ProductBuilder[D] =
      new ProductBuilder[D] {
        val d: ConfigDescriptor[KK, VV, D] = dd
      }

    sealed abstract class ProductBuilder[D] {
      val d: ConfigDescriptor[KK, VV, D]
      def apply[E](ff: (A, B, C, D) => E, gg: E => Option[(A, B, C, D)]): ConfigDescriptor[KK, VV, E] =
        (a zip b zip c zip d)
          .xmapEither[KK, VV, E] {
            case (((aa, bb), cc), dd) => Right(ff(aa, bb, cc, dd))
          }(
            liftWrite(e => gg(e).map { case (aa, bb, cc, dd) => (((aa, bb), cc), dd) })
          )

      def |@|[E](ee: ConfigDescriptor[KK, VV, E]): ProductBuilder[E] =
        new ProductBuilder[E] {
          val e: ConfigDescriptor[KK, VV, E] = ee
        }

      sealed abstract class ProductBuilder[E] {
        val e: ConfigDescriptor[KK, VV, E]
        def apply[G](ff: (A, B, C, D, E) => G, gg: G => Option[(A, B, C, D, E)]): ConfigDescriptor[KK, VV, G] =
          (a zip b zip c zip d zip e)
            .xmapEither[KK, VV, G] {
              case ((((aa, bb), cc), dd), ee) => Right(ff(aa, bb, cc, dd, ee))
            }(
              liftWrite(g => gg(g).map { case (aa, bb, cc, dd, ee) => ((((aa, bb), cc), dd), ee) })
            )

        def |@|[G](gg: ConfigDescriptor[KK, VV, G]): ProductBuilder[G] =
          new ProductBuilder[G] {
            val g: ConfigDescriptor[KK, VV, G] = gg
          }

        sealed abstract class ProductBuilder[G] {
          val g: ConfigDescriptor[KK, VV, G]
          def apply[H](ff: (A, B, C, D, E, G) => H, gg: H => Option[(A, B, C, D, E, G)]): ConfigDescriptor[KK, VV, H] =
            (a zip b zip c zip d zip e zip g)
              .xmapEither[KK, VV, H] {
                case (((((aa, bb), cc), dd), ee), gg) => Right(ff(aa, bb, cc, dd, ee, gg))
              }(
                liftWrite(h => gg(h).map { case (aa, bb, cc, dd, ee, gg) => (((((aa, bb), cc), dd), ee), gg) })
              )

          def |@|[H](hh: ConfigDescriptor[KK, VV, H]): ProductBuilder[H] =
            new ProductBuilder[H] {
              val h: ConfigDescriptor[KK, VV, H] = hh
            }

          sealed abstract class ProductBuilder[H] {
            val h: ConfigDescriptor[KK, VV, H]
            def apply[I](ff: (A, B, C, D, E, G, H) => I, gg: I => Option[(A, B, C, D, E, G, H)]): ConfigDescriptor[KK, VV, I] =
              (a zip b zip c zip d zip e zip g zip h)
                .xmapEither[KK, VV, I] {
                  case ((((((aa, bb), cc), dd), ee), gg), hh) => Right(ff(aa, bb, cc, dd, ee, gg, hh))
                }(
                  liftWrite(i => gg(i).map { case (aa, bb, cc, dd, ee, gg, hh) => ((((((aa, bb), cc), dd), ee), gg), hh) })
                )

            def |@|[I](ii: ConfigDescriptor[KK, VV, I]): ProductBuilder[I] =
              new ProductBuilder[I] {
                val i: ConfigDescriptor[KK, VV, I] = ii
              }

            sealed abstract class ProductBuilder[I] {
              val i: ConfigDescriptor[KK, VV, I]
              def apply[J](ff: (A, B, C, D, E, G, H, I) => J, gg: J => Option[(A, B, C, D, E, G, H, I)]): ConfigDescriptor[KK, VV, J] =
                (a zip b zip c zip d zip e zip g zip h zip i)
                  .xmapEither[KK, VV, J] {
                    case (((((((aa, bb), cc), dd), ee), gg), hh), ii) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii))
                  }(
                    liftWrite(j => gg(j).map { case (aa, bb, cc, dd, ee, gg, hh, ii) => (((((((aa, bb), cc), dd), ee), gg), hh), ii) })
                  )

              def |@|[J](jj: ConfigDescriptor[KK, VV, J]): ProductBuilder[J] =
                new ProductBuilder[J] {
                  val j: ConfigDescriptor[KK, VV, J] = jj
                }

              sealed abstract class ProductBuilder[J] {
                val j: ConfigDescriptor[KK, VV, J]
                def apply[K](ff: (A, B, C, D, E, G, H, I, J) => K, gg: K => Option[(A, B, C, D, E, G, H, I, J)]): ConfigDescriptor[KK, VV, K] =
                  (a zip b zip c zip d zip e zip g zip h zip i zip j)
                    .xmapEither[KK, VV, K] {
                      case ((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj))
                    }(
                      liftWrite(k => gg(k).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj) => ((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj) })
                    )

                def |@|[K](kk: ConfigDescriptor[KK, VV, K]): ProductBuilder[K] =
                  new ProductBuilder[K] {
                    val k: ConfigDescriptor[KK, VV, K] = kk
                  }

                sealed abstract class ProductBuilder[K] {
                  val k: ConfigDescriptor[KK, VV, K]
                  def apply[L](ff: (A, B, C, D, E, G, H, I, J, K) => L, gg: L => Option[(A, B, C, D, E, G, H, I, J, K)]): ConfigDescriptor[KK, VV, L] =
                    (a zip b zip c zip d zip e zip g zip h zip i zip j zip k)
                      .xmapEither[KK, VV, L] {
                        case (((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk))
                      }(
                        liftWrite(l => gg(l).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk) => (((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk) })
                      )

                  def |@|[L](ll: ConfigDescriptor[KK, VV, L]): ProductBuilder[L] =
                    new ProductBuilder[L] {
                      val l: ConfigDescriptor[KK, VV, L] = ll
                    }

                  sealed abstract class ProductBuilder[L] {
                    val l: ConfigDescriptor[KK, VV, L]
                    def apply[M](ff: (A, B, C, D, E, G, H, I, J, K, L) => M, gg: M => Option[(A, B, C, D, E, G, H, I, J, K, L)]): ConfigDescriptor[KK, VV, M] =
                      (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l)
                        .xmapEither[KK, VV, M] {
                          case ((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll))
                        }(
                          liftWrite(m => gg(m).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll) => ((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll) })
                        )

                    def |@|[M](mm: ConfigDescriptor[KK, VV, M]): ProductBuilder[M] =
                      new ProductBuilder[M] {
                        val m: ConfigDescriptor[KK, VV, M] = mm
                      }

                    sealed abstract class ProductBuilder[M] {
                      val m: ConfigDescriptor[KK, VV, M]
                      def apply[N](ff: (A, B, C, D, E, G, H, I, J, K, L, M) => N, gg: N => Option[(A, B, C, D, E, G, H, I, J, K, L, M)]): ConfigDescriptor[KK, VV, N] =
                        (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m)
                          .xmapEither[KK, VV, N] {
                            case (((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm))
                          }(
                            liftWrite(n => gg(n).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm) => (((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm) })
                          )

                      def |@|[N](nn: ConfigDescriptor[KK, VV, N]): ProductBuilder[N] =
                        new ProductBuilder[N] {
                          val n: ConfigDescriptor[KK, VV, N] = nn
                        }

                      sealed abstract class ProductBuilder[N] {
                        val n: ConfigDescriptor[KK, VV, N]
                        def apply[O](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N) => O, gg: O => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N)]): ConfigDescriptor[KK, VV, O] =
                          (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n)
                            .xmapEither[KK, VV, O] {
                              case ((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn))
                            }(
                              liftWrite(o => gg(o).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn) => ((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn) })
                            )

                        def |@|[O](oo: ConfigDescriptor[KK, VV, O]): ProductBuilder[O] =
                          new ProductBuilder[O] {
                            val o: ConfigDescriptor[KK, VV, O] = oo
                          }

                        sealed abstract class ProductBuilder[O] {
                          val o: ConfigDescriptor[KK, VV, O]
                          def apply[P](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O) => P, gg: P => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O)]): ConfigDescriptor[KK, VV, P] =
                            (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o)
                              .xmapEither[KK, VV, P] {
                                case (((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo))
                              }(
                                liftWrite(p => gg(p).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo) => (((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo) })
                              )

                          def |@|[P](pp: ConfigDescriptor[KK, VV, P]): ProductBuilder[P] =
                            new ProductBuilder[P] {
                              val p: ConfigDescriptor[KK, VV, P] = pp
                            }

                          sealed abstract class ProductBuilder[P] {
                            val p: ConfigDescriptor[KK, VV, P]
                            def apply[Q](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P) => Q, gg: Q => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P)]): ConfigDescriptor[KK, VV, Q] =
                              (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p)
                                .xmapEither[KK, VV, Q] {
                                  case ((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp))
                                }(
                                  liftWrite(q => gg(q).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp) => ((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp) })
                                )

                            def |@|[Q](qq: ConfigDescriptor[KK, VV, Q]): ProductBuilder[Q] =
                              new ProductBuilder[Q] {
                                val q: ConfigDescriptor[KK, VV, Q] = qq
                              }

                            sealed abstract class ProductBuilder[Q] {
                              val q: ConfigDescriptor[KK, VV, Q]
                              def apply[R](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q) => R, gg: R => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q)]): ConfigDescriptor[KK, VV, R] =
                                (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q)
                                  .xmapEither[KK, VV, R] {
                                    case (((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq))
                                  }(
                                    liftWrite(r => gg(r).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq) => (((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq) })
                                  )

                              def |@|[R](rr: ConfigDescriptor[KK, VV, R]): ProductBuilder[R] =
                                new ProductBuilder[R] {
                                  val r: ConfigDescriptor[KK, VV, R] = rr
                                }

                              sealed abstract class ProductBuilder[R] {
                                val r: ConfigDescriptor[KK, VV, R]
                                def apply[S](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R) => S, gg: S => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R)]): ConfigDescriptor[KK, VV, S] =
                                  (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r)
                                    .xmapEither[KK, VV, S] {
                                      case ((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr))
                                    }(
                                      liftWrite(s => gg(s).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr) => ((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr) })
                                    )

                                def |@|[S](ss: ConfigDescriptor[KK, VV, S]): ProductBuilder[S] =
                                  new ProductBuilder[S] {
                                    val s: ConfigDescriptor[KK, VV, S] = ss
                                  }

                                sealed abstract class ProductBuilder[S] {
                                  val s: ConfigDescriptor[KK, VV, S]
                                  def apply[T](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T, gg: T => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S)]): ConfigDescriptor[KK, VV, T] =
                                    (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s)
                                      .xmapEither[KK, VV, T] {
                                        case (((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss))
                                      }(
                                        liftWrite(t => gg(t).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss) => (((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss) })
                                      )

                                  def |@|[T](tt: ConfigDescriptor[KK, VV, T]): ProductBuilder[T] =
                                    new ProductBuilder[T] {
                                      val t: ConfigDescriptor[KK, VV, T] = tt
                                    }

                                  sealed abstract class ProductBuilder[T] {
                                    val t: ConfigDescriptor[KK, VV, T]
                                    def apply[U](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U, gg: U => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]): ConfigDescriptor[KK, VV, U] =
                                      (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t)
                                        .xmapEither[KK, VV, U] {
                                          case ((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt))
                                        }(
                                          liftWrite(u => gg(u).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt) => ((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt) })
                                        )

                                    def |@|[U](uu: ConfigDescriptor[KK, VV, U]): ProductBuilder[U] =
                                      new ProductBuilder[U] {
                                        val u: ConfigDescriptor[KK, VV, U] = uu
                                      }

                                    sealed abstract class ProductBuilder[U] {
                                      val u: ConfigDescriptor[KK, VV, U]
                                      def apply[V](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V, gg: V => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]): ConfigDescriptor[KK, VV, V] =
                                        (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t zip u)
                                          .xmapEither[KK, VV, V] {
                                            case (((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt), uu) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt, uu))
                                          }(
                                            liftWrite(v => gg(v).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt, uu) => (((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt), uu) })
                                          )

                                      def |@|[V](vv: ConfigDescriptor[KK, VV, V]): ProductBuilder[V] =
                                        new ProductBuilder[V] {
                                          val v: ConfigDescriptor[KK, VV, V] = vv
                                        }

                                      sealed abstract class ProductBuilder[V] {
                                        val v: ConfigDescriptor[KK, VV, V]
                                        def apply[W](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W, gg: W => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]): ConfigDescriptor[KK, VV, W] =
                                          (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t zip u zip v)
                                            .xmapEither[KK, VV, W] {
                                              case ((((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt), uu), vv) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt, uu, vv))
                                            }(
                                              liftWrite(w => gg(w).map { case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt, uu, vv) => ((((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt), uu), vv) })
                                            )

                                        def |@|[W](ww: ConfigDescriptor[KK, VV, W]): ProductBuilder[W] =
                                          new ProductBuilder[W] {
                                            val w: ConfigDescriptor[KK, VV, W] = ww
                                          }

                                        sealed abstract class ProductBuilder[W] {
                                          val w: ConfigDescriptor[KK, VV, W]
                                          def apply[X](ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) => X, gg: X => Option[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W)]): ConfigDescriptor[KK, VV, X] =
                                            (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t zip u zip v zip w)
                                              .xmapEither[KK, VV, X] {
                                                case (((((((((((((((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj), kk), ll), mm), nn), oo), pp), qq), rr), ss), tt), uu), vv), ww) => Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq, rr, ss, tt, uu, vv, ww))
                                              }(
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
