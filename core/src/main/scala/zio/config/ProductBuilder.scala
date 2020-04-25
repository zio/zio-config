package zio.config

import ProductBuilder._

private[config] trait ProductBuilder[F[_], A, B] { self =>

  def zip[X, Y]: (F[X], F[Y]) => F[(X, Y)]
  def xmapEither[X, Y]
    : (F[X], X => Either[String, Y], Y => Either[String, X]) => F[Y]

  implicit class Syntax[X](x: F[X]) {
    def zip[Y](y: F[Y]): F[(X, Y)] = self.zip(x, y)
    def xmapEither[Y](f: X => Either[String, Y],
                      g: Y => Either[String, X]): F[Y] =
      self.xmapEither(x, f, g)
  }

  val a: F[A]
  val b: F[B]

  def tupled =
    apply[(A, B)]((a: A, b: B) => (a, b), (t: (A, B)) => Some((t._1, t._2)))

  def apply[C](f: (A, B) => C, g: C => Option[(A, B)]): F[C] =
    a.zip(b).xmapEither({ case (aa, bb) => Right(f(aa, bb)) }, liftWrite(g))

  def |@|[C](cc: F[C]): ProductBuilder[C] = new ProductBuilder[C] {
    val c: F[C] = cc
  }

  // /start/productbuilder/ (used by codegen - DO NOT REMOVE)

  sealed abstract class ProductBuilder[C] {
    val c: F[C]

    def apply[D](ff: (A, B, C) => D, gg: D => Option[(A, B, C)]): F[D] =
      (a zip b zip c)
        .xmapEither[D](
          { case ((aa, bb), cc) => Right(ff(aa, bb, cc)) },
          liftWrite(d => gg(d).map { case (aa, bb, cc) => ((aa, bb), cc) })
        )

    def tupled =
      apply[(A, B, C)](
        (a: A, b: B, c: C) => (a, b, c),
        t => Some((t._1, t._2, t._3))
      )

    def |@|[D](dd: F[D]): ProductBuilder[D] =
      new ProductBuilder[D] {
        val d: F[D] = dd
      }

    sealed abstract class ProductBuilder[D] {
      val d: F[D]

      def apply[E](ff: (A, B, C, D) => E, gg: E => Option[(A, B, C, D)]): F[E] =
        (a zip b zip c zip d)
          .xmapEither[E](
            { case (((aa, bb), cc), dd) => Right(ff(aa, bb, cc, dd)) },
            liftWrite(
              e => gg(e).map { case (aa, bb, cc, dd) => (((aa, bb), cc), dd) }
            )
          )

      def tupled =
        apply[(A, B, C, D)](
          (a: A, b: B, c: C, d: D) => (a, b, c, d),
          t => Some((t._1, t._2, t._3, t._4))
        )

      def |@|[E](ee: F[E]): ProductBuilder[E] =
        new ProductBuilder[E] {
          val e: F[E] = ee
        }

      sealed abstract class ProductBuilder[E] {
        val e: F[E]

        def apply[G](ff: (A, B, C, D, E) => G,
                     gg: G => Option[(A, B, C, D, E)]): F[G] =
          (a zip b zip c zip d zip e)
            .xmapEither[G](
              {
                case ((((aa, bb), cc), dd), ee) => Right(ff(aa, bb, cc, dd, ee))
              },
              liftWrite(
                g =>
                  gg(g).map {
                    case (aa, bb, cc, dd, ee) => ((((aa, bb), cc), dd), ee)
                }
              )
            )

        def tupled =
          apply[(A, B, C, D, E)](
            (a: A, b: B, c: C, d: D, e: E) => (a, b, c, d, e),
            t => Some((t._1, t._2, t._3, t._4, t._5))
          )

        def |@|[G](gg: F[G]): ProductBuilder[G] =
          new ProductBuilder[G] {
            val g: F[G] = gg
          }

        sealed abstract class ProductBuilder[G] {
          val g: F[G]

          def apply[H](ff: (A, B, C, D, E, G) => H,
                       gg: H => Option[(A, B, C, D, E, G)]): F[H] =
            (a zip b zip c zip d zip e zip g)
              .xmapEither[H](
                {
                  case (((((aa, bb), cc), dd), ee), gg) =>
                    Right(ff(aa, bb, cc, dd, ee, gg))
                },
                liftWrite(
                  h =>
                    gg(h).map {
                      case (aa, bb, cc, dd, ee, gg) =>
                        (((((aa, bb), cc), dd), ee), gg)
                  }
                )
              )

          def tupled =
            apply[(A, B, C, D, E, G)](
              (a: A, b: B, c: C, d: D, e: E, g: G) => (a, b, c, d, e, g),
              t => Some((t._1, t._2, t._3, t._4, t._5, t._6))
            )

          def |@|[H](hh: F[H]): ProductBuilder[H] =
            new ProductBuilder[H] {
              val h: F[H] = hh
            }

          sealed abstract class ProductBuilder[H] {
            val h: F[H]

            def apply[I](ff: (A, B, C, D, E, G, H) => I,
                         gg: I => Option[(A, B, C, D, E, G, H)]): F[I] =
              (a zip b zip c zip d zip e zip g zip h)
                .xmapEither[I](
                  {
                    case ((((((aa, bb), cc), dd), ee), gg), hh) =>
                      Right(ff(aa, bb, cc, dd, ee, gg, hh))
                  },
                  liftWrite(
                    i =>
                      gg(i).map {
                        case (aa, bb, cc, dd, ee, gg, hh) =>
                          ((((((aa, bb), cc), dd), ee), gg), hh)
                    }
                  )
                )

            def tupled =
              apply[(A, B, C, D, E, G, H)](
                (a: A, b: B, c: C, d: D, e: E, g: G, h: H) =>
                  (a, b, c, d, e, g, h),
                t => Some((t._1, t._2, t._3, t._4, t._5, t._6, t._7))
              )

            def |@|[I](ii: F[I]): ProductBuilder[I] =
              new ProductBuilder[I] {
                val i: F[I] = ii
              }

            sealed abstract class ProductBuilder[I] {
              val i: F[I]

              def apply[J](ff: (A, B, C, D, E, G, H, I) => J,
                           gg: J => Option[(A, B, C, D, E, G, H, I)]): F[J] =
                (a zip b zip c zip d zip e zip g zip h zip i)
                  .xmapEither[J](
                    {
                      case (((((((aa, bb), cc), dd), ee), gg), hh), ii) =>
                        Right(ff(aa, bb, cc, dd, ee, gg, hh, ii))
                    },
                    liftWrite(
                      j =>
                        gg(j).map {
                          case (aa, bb, cc, dd, ee, gg, hh, ii) =>
                            (((((((aa, bb), cc), dd), ee), gg), hh), ii)
                      }
                    )
                  )

              def tupled =
                apply[(A, B, C, D, E, G, H, I)](
                  (a: A, b: B, c: C, d: D, e: E, g: G, h: H, i: I) =>
                    (a, b, c, d, e, g, h, i),
                  t => Some((t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8))
                )

              def |@|[J](jj: F[J]): ProductBuilder[J] =
                new ProductBuilder[J] {
                  val j: F[J] = jj
                }

              sealed abstract class ProductBuilder[J] {
                val j: F[J]

                def apply[K](
                  ff: (A, B, C, D, E, G, H, I, J) => K,
                  gg: K => Option[(A, B, C, D, E, G, H, I, J)]
                ): F[K] =
                  (a zip b zip c zip d zip e zip g zip h zip i zip j)
                    .xmapEither[K](
                      {
                        case (
                            (((((((aa, bb), cc), dd), ee), gg), hh), ii),
                            jj
                            ) =>
                          Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj))
                      },
                      liftWrite(
                        k =>
                          gg(k).map {
                            case (aa, bb, cc, dd, ee, gg, hh, ii, jj) =>
                              ((((((((aa, bb), cc), dd), ee), gg), hh), ii), jj)
                        }
                      )
                    )

                def tupled =
                  apply[(A, B, C, D, E, G, H, I, J)](
                    (a: A, b: B, c: C, d: D, e: E, g: G, h: H, i: I, j: J) =>
                      (a, b, c, d, e, g, h, i, j),
                    t =>
                      Some(
                        (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
                    )
                  )

                def |@|[K](kk: F[K]): ProductBuilder[K] =
                  new ProductBuilder[K] {
                    val k: F[K] = kk
                  }

                sealed abstract class ProductBuilder[K] {
                  val k: F[K]

                  def apply[L](
                    ff: (A, B, C, D, E, G, H, I, J, K) => L,
                    gg: L => Option[(A, B, C, D, E, G, H, I, J, K)]
                  ): F[L] =
                    (a zip b zip c zip d zip e zip g zip h zip i zip j zip k)
                      .xmapEither[L](
                        {
                          case (
                              (
                                (((((((aa, bb), cc), dd), ee), gg), hh), ii),
                                jj
                              ),
                              kk
                              ) =>
                            Right(ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk))
                        },
                        liftWrite(
                          l =>
                            gg(l).map {
                              case (aa, bb, cc, dd, ee, gg, hh, ii, jj, kk) =>
                                (
                                  (
                                    (
                                      ((((((aa, bb), cc), dd), ee), gg), hh),
                                      ii
                                    ),
                                    jj
                                  ),
                                  kk
                                )
                          }
                        )
                      )

                  def tupled =
                    apply[(A, B, C, D, E, G, H, I, J, K)](
                      (a: A,
                       b: B,
                       c: C,
                       d: D,
                       e: E,
                       g: G,
                       h: H,
                       i: I,
                       j: J,
                       k: K) => (a, b, c, d, e, g, h, i, j, k),
                      t =>
                        Some(
                          (
                            t._1,
                            t._2,
                            t._3,
                            t._4,
                            t._5,
                            t._6,
                            t._7,
                            t._8,
                            t._9,
                            t._10
                          )
                      )
                    )

                  def |@|[L](ll: F[L]): ProductBuilder[L] =
                    new ProductBuilder[L] {
                      val l: F[L] = ll
                    }

                  sealed abstract class ProductBuilder[L] {
                    val l: F[L]

                    def apply[M](
                      ff: (A, B, C, D, E, G, H, I, J, K, L) => M,
                      gg: M => Option[(A, B, C, D, E, G, H, I, J, K, L)]
                    ): F[M] =
                      (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l)
                        .xmapEither[M](
                          {
                            case (
                                (
                                  (
                                    (
                                      ((((((aa, bb), cc), dd), ee), gg), hh),
                                      ii
                                    ),
                                    jj
                                  ),
                                  kk
                                ),
                                ll
                                ) =>
                              Right(
                                ff(aa, bb, cc, dd, ee, gg, hh, ii, jj, kk, ll)
                              )
                          },
                          liftWrite(
                            m =>
                              gg(m).map {
                                case (
                                    aa,
                                    bb,
                                    cc,
                                    dd,
                                    ee,
                                    gg,
                                    hh,
                                    ii,
                                    jj,
                                    kk,
                                    ll
                                    ) =>
                                  (
                                    (
                                      (
                                        (
                                          (
                                            (((((aa, bb), cc), dd), ee), gg),
                                            hh
                                          ),
                                          ii
                                        ),
                                        jj
                                      ),
                                      kk
                                    ),
                                    ll
                                  )
                            }
                          )
                        )

                    def tupled =
                      apply[(A, B, C, D, E, G, H, I, J, K, L)](
                        (a: A,
                         b: B,
                         c: C,
                         d: D,
                         e: E,
                         g: G,
                         h: H,
                         i: I,
                         j: J,
                         k: K,
                         l: L) => (a, b, c, d, e, g, h, i, j, k, l),
                        t =>
                          Some(
                            (
                              t._1,
                              t._2,
                              t._3,
                              t._4,
                              t._5,
                              t._6,
                              t._7,
                              t._8,
                              t._9,
                              t._10,
                              t._11
                            )
                        )
                      )

                    def |@|[M](mm: F[M]): ProductBuilder[M] =
                      new ProductBuilder[M] {
                        val m: F[M] = mm
                      }

                    sealed abstract class ProductBuilder[M] {
                      val m: F[M]

                      def apply[N](
                        ff: (A, B, C, D, E, G, H, I, J, K, L, M) => N,
                        gg: N => Option[(A, B, C, D, E, G, H, I, J, K, L, M)]
                      ): F[N] =
                        (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m)
                          .xmapEither[N](
                            {
                              case (
                                  (
                                    (
                                      (
                                        (
                                          (
                                            (((((aa, bb), cc), dd), ee), gg),
                                            hh
                                          ),
                                          ii
                                        ),
                                        jj
                                      ),
                                      kk
                                    ),
                                    ll
                                  ),
                                  mm
                                  ) =>
                                Right(
                                  ff(
                                    aa,
                                    bb,
                                    cc,
                                    dd,
                                    ee,
                                    gg,
                                    hh,
                                    ii,
                                    jj,
                                    kk,
                                    ll,
                                    mm
                                  )
                                )
                            },
                            liftWrite(
                              n =>
                                gg(n).map {
                                  case (
                                      aa,
                                      bb,
                                      cc,
                                      dd,
                                      ee,
                                      gg,
                                      hh,
                                      ii,
                                      jj,
                                      kk,
                                      ll,
                                      mm
                                      ) =>
                                    (
                                      (
                                        (
                                          (
                                            (
                                              (
                                                (
                                                  ((((aa, bb), cc), dd), ee),
                                                  gg
                                                ),
                                                hh
                                              ),
                                              ii
                                            ),
                                            jj
                                          ),
                                          kk
                                        ),
                                        ll
                                      ),
                                      mm
                                    )
                              }
                            )
                          )

                      def tupled =
                        apply[(A, B, C, D, E, G, H, I, J, K, L, M)](
                          (a: A,
                           b: B,
                           c: C,
                           d: D,
                           e: E,
                           g: G,
                           h: H,
                           i: I,
                           j: J,
                           k: K,
                           l: L,
                           m: M) => (a, b, c, d, e, g, h, i, j, k, l, m),
                          t =>
                            Some(
                              (
                                t._1,
                                t._2,
                                t._3,
                                t._4,
                                t._5,
                                t._6,
                                t._7,
                                t._8,
                                t._9,
                                t._10,
                                t._11,
                                t._12
                              )
                          )
                        )

                      def |@|[N](nn: F[N]): ProductBuilder[N] =
                        new ProductBuilder[N] {
                          val n: F[N] = nn
                        }

                      sealed abstract class ProductBuilder[N] {
                        val n: F[N]

                        def apply[O](
                          ff: (A, B, C, D, E, G, H, I, J, K, L, M, N) => O,
                          gg: O => Option[
                            (A, B, C, D, E, G, H, I, J, K, L, M, N)
                          ]
                        ): F[O] =
                          (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n)
                            .xmapEither[O](
                              {
                                case (
                                    (
                                      (
                                        (
                                          (
                                            (
                                              (
                                                (
                                                  ((((aa, bb), cc), dd), ee),
                                                  gg
                                                ),
                                                hh
                                              ),
                                              ii
                                            ),
                                            jj
                                          ),
                                          kk
                                        ),
                                        ll
                                      ),
                                      mm
                                    ),
                                    nn
                                    ) =>
                                  Right(
                                    ff(
                                      aa,
                                      bb,
                                      cc,
                                      dd,
                                      ee,
                                      gg,
                                      hh,
                                      ii,
                                      jj,
                                      kk,
                                      ll,
                                      mm,
                                      nn
                                    )
                                  )
                              },
                              liftWrite(
                                o =>
                                  gg(o).map {
                                    case (
                                        aa,
                                        bb,
                                        cc,
                                        dd,
                                        ee,
                                        gg,
                                        hh,
                                        ii,
                                        jj,
                                        kk,
                                        ll,
                                        mm,
                                        nn
                                        ) =>
                                      (
                                        (
                                          (
                                            (
                                              (
                                                (
                                                  (
                                                    (
                                                      (
                                                        (((aa, bb), cc), dd),
                                                        ee
                                                      ),
                                                      gg
                                                    ),
                                                    hh
                                                  ),
                                                  ii
                                                ),
                                                jj
                                              ),
                                              kk
                                            ),
                                            ll
                                          ),
                                          mm
                                        ),
                                        nn
                                      )
                                }
                              )
                            )

                        def tupled =
                          apply[(A, B, C, D, E, G, H, I, J, K, L, M, N)](
                            (a: A,
                             b: B,
                             c: C,
                             d: D,
                             e: E,
                             g: G,
                             h: H,
                             i: I,
                             j: J,
                             k: K,
                             l: L,
                             m: M,
                             n: N) => (a, b, c, d, e, g, h, i, j, k, l, m, n),
                            t =>
                              Some(
                                (
                                  t._1,
                                  t._2,
                                  t._3,
                                  t._4,
                                  t._5,
                                  t._6,
                                  t._7,
                                  t._8,
                                  t._9,
                                  t._10,
                                  t._11,
                                  t._12,
                                  t._13
                                )
                            )
                          )

                        def |@|[O](oo: F[O]): ProductBuilder[O] =
                          new ProductBuilder[O] {
                            val o: F[O] = oo
                          }

                        sealed abstract class ProductBuilder[O] {
                          val o: F[O]

                          def apply[P](
                            ff: (A, B, C, D, E, G, H, I, J, K, L, M, N, O) => P,
                            gg: P => Option[
                              (A, B, C, D, E, G, H, I, J, K, L, M, N, O)
                            ]
                          ): F[P] =
                            (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o)
                              .xmapEither[P](
                                {
                                  case (
                                      (
                                        (
                                          (
                                            (
                                              (
                                                (
                                                  (
                                                    (
                                                      (
                                                        (((aa, bb), cc), dd),
                                                        ee
                                                      ),
                                                      gg
                                                    ),
                                                    hh
                                                  ),
                                                  ii
                                                ),
                                                jj
                                              ),
                                              kk
                                            ),
                                            ll
                                          ),
                                          mm
                                        ),
                                        nn
                                      ),
                                      oo
                                      ) =>
                                    Right(
                                      ff(
                                        aa,
                                        bb,
                                        cc,
                                        dd,
                                        ee,
                                        gg,
                                        hh,
                                        ii,
                                        jj,
                                        kk,
                                        ll,
                                        mm,
                                        nn,
                                        oo
                                      )
                                    )
                                },
                                liftWrite(
                                  p =>
                                    gg(p).map {
                                      case (
                                          aa,
                                          bb,
                                          cc,
                                          dd,
                                          ee,
                                          gg,
                                          hh,
                                          ii,
                                          jj,
                                          kk,
                                          ll,
                                          mm,
                                          nn,
                                          oo
                                          ) =>
                                        (
                                          (
                                            (
                                              (
                                                (
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              ((aa, bb), cc),
                                                              dd
                                                            ),
                                                            ee
                                                          ),
                                                          gg
                                                        ),
                                                        hh
                                                      ),
                                                      ii
                                                    ),
                                                    jj
                                                  ),
                                                  kk
                                                ),
                                                ll
                                              ),
                                              mm
                                            ),
                                            nn
                                          ),
                                          oo
                                        )
                                  }
                                )
                              )

                          def tupled =
                            apply[(A, B, C, D, E, G, H, I, J, K, L, M, N, O)](
                              (a: A,
                               b: B,
                               c: C,
                               d: D,
                               e: E,
                               g: G,
                               h: H,
                               i: I,
                               j: J,
                               k: K,
                               l: L,
                               m: M,
                               n: N,
                               o: O) =>
                                (a, b, c, d, e, g, h, i, j, k, l, m, n, o),
                              t =>
                                Some(
                                  (
                                    t._1,
                                    t._2,
                                    t._3,
                                    t._4,
                                    t._5,
                                    t._6,
                                    t._7,
                                    t._8,
                                    t._9,
                                    t._10,
                                    t._11,
                                    t._12,
                                    t._13,
                                    t._14
                                  )
                              )
                            )

                          def |@|[P](pp: F[P]): ProductBuilder[P] =
                            new ProductBuilder[P] {
                              val p: F[P] = pp
                            }

                          sealed abstract class ProductBuilder[P] {
                            val p: F[P]

                            def apply[Q](
                              ff: (A,
                                   B,
                                   C,
                                   D,
                                   E,
                                   G,
                                   H,
                                   I,
                                   J,
                                   K,
                                   L,
                                   M,
                                   N,
                                   O,
                                   P) => Q,
                              gg: Q => Option[
                                (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P)
                              ]
                            ): F[Q] =
                              (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p)
                                .xmapEither[Q](
                                  {
                                    case (
                                        (
                                          (
                                            (
                                              (
                                                (
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              ((aa, bb), cc),
                                                              dd
                                                            ),
                                                            ee
                                                          ),
                                                          gg
                                                        ),
                                                        hh
                                                      ),
                                                      ii
                                                    ),
                                                    jj
                                                  ),
                                                  kk
                                                ),
                                                ll
                                              ),
                                              mm
                                            ),
                                            nn
                                          ),
                                          oo
                                        ),
                                        pp
                                        ) =>
                                      Right(
                                        ff(
                                          aa,
                                          bb,
                                          cc,
                                          dd,
                                          ee,
                                          gg,
                                          hh,
                                          ii,
                                          jj,
                                          kk,
                                          ll,
                                          mm,
                                          nn,
                                          oo,
                                          pp
                                        )
                                      )
                                  },
                                  liftWrite(
                                    q =>
                                      gg(q).map {
                                        case (
                                            aa,
                                            bb,
                                            cc,
                                            dd,
                                            ee,
                                            gg,
                                            hh,
                                            ii,
                                            jj,
                                            kk,
                                            ll,
                                            mm,
                                            nn,
                                            oo,
                                            pp
                                            ) =>
                                          (
                                            (
                                              (
                                                (
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (aa, bb),
                                                                    cc
                                                                  ),
                                                                  dd
                                                                ),
                                                                ee
                                                              ),
                                                              gg
                                                            ),
                                                            hh
                                                          ),
                                                          ii
                                                        ),
                                                        jj
                                                      ),
                                                      kk
                                                    ),
                                                    ll
                                                  ),
                                                  mm
                                                ),
                                                nn
                                              ),
                                              oo
                                            ),
                                            pp
                                          )
                                    }
                                  )
                                )

                            def tupled =
                              apply[
                                (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P)
                              ](
                                (a: A,
                                 b: B,
                                 c: C,
                                 d: D,
                                 e: E,
                                 g: G,
                                 h: H,
                                 i: I,
                                 j: J,
                                 k: K,
                                 l: L,
                                 m: M,
                                 n: N,
                                 o: O,
                                 p: P) =>
                                  (a, b, c, d, e, g, h, i, j, k, l, m, n, o, p),
                                t =>
                                  Some(
                                    (
                                      t._1,
                                      t._2,
                                      t._3,
                                      t._4,
                                      t._5,
                                      t._6,
                                      t._7,
                                      t._8,
                                      t._9,
                                      t._10,
                                      t._11,
                                      t._12,
                                      t._13,
                                      t._14,
                                      t._15
                                    )
                                )
                              )

                            def |@|[Q](qq: F[Q]): ProductBuilder[Q] =
                              new ProductBuilder[Q] {
                                val q: F[Q] = qq
                              }

                            sealed abstract class ProductBuilder[Q] {
                              val q: F[Q]

                              def apply[R](ff: (A,
                                                B,
                                                C,
                                                D,
                                                E,
                                                G,
                                                H,
                                                I,
                                                J,
                                                K,
                                                L,
                                                M,
                                                N,
                                                O,
                                                P,
                                                Q) => R,
                                           gg: R => Option[
                                             (A,
                                              B,
                                              C,
                                              D,
                                              E,
                                              G,
                                              H,
                                              I,
                                              J,
                                              K,
                                              L,
                                              M,
                                              N,
                                              O,
                                              P,
                                              Q)
                                           ]): F[R] =
                                (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q)
                                  .xmapEither[R](
                                    {
                                      case (
                                          (
                                            (
                                              (
                                                (
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (aa, bb),
                                                                    cc
                                                                  ),
                                                                  dd
                                                                ),
                                                                ee
                                                              ),
                                                              gg
                                                            ),
                                                            hh
                                                          ),
                                                          ii
                                                        ),
                                                        jj
                                                      ),
                                                      kk
                                                    ),
                                                    ll
                                                  ),
                                                  mm
                                                ),
                                                nn
                                              ),
                                              oo
                                            ),
                                            pp
                                          ),
                                          qq
                                          ) =>
                                        Right(
                                          ff(
                                            aa,
                                            bb,
                                            cc,
                                            dd,
                                            ee,
                                            gg,
                                            hh,
                                            ii,
                                            jj,
                                            kk,
                                            ll,
                                            mm,
                                            nn,
                                            oo,
                                            pp,
                                            qq
                                          )
                                        )
                                    },
                                    liftWrite(
                                      r =>
                                        gg(r).map {
                                          case (
                                              aa,
                                              bb,
                                              cc,
                                              dd,
                                              ee,
                                              gg,
                                              hh,
                                              ii,
                                              jj,
                                              kk,
                                              ll,
                                              mm,
                                              nn,
                                              oo,
                                              pp,
                                              qq
                                              ) =>
                                            (
                                              (
                                                (
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          aa,
                                                                          bb
                                                                        ),
                                                                        cc
                                                                      ),
                                                                      dd
                                                                    ),
                                                                    ee
                                                                  ),
                                                                  gg
                                                                ),
                                                                hh
                                                              ),
                                                              ii
                                                            ),
                                                            jj
                                                          ),
                                                          kk
                                                        ),
                                                        ll
                                                      ),
                                                      mm
                                                    ),
                                                    nn
                                                  ),
                                                  oo
                                                ),
                                                pp
                                              ),
                                              qq
                                            )
                                      }
                                    )
                                  )

                              def tupled =
                                apply[
                                  (A,
                                   B,
                                   C,
                                   D,
                                   E,
                                   G,
                                   H,
                                   I,
                                   J,
                                   K,
                                   L,
                                   M,
                                   N,
                                   O,
                                   P,
                                   Q)
                                ](
                                  (a: A,
                                   b: B,
                                   c: C,
                                   d: D,
                                   e: E,
                                   g: G,
                                   h: H,
                                   i: I,
                                   j: J,
                                   k: K,
                                   l: L,
                                   m: M,
                                   n: N,
                                   o: O,
                                   p: P,
                                   q: Q) =>
                                    (
                                      a,
                                      b,
                                      c,
                                      d,
                                      e,
                                      g,
                                      h,
                                      i,
                                      j,
                                      k,
                                      l,
                                      m,
                                      n,
                                      o,
                                      p,
                                      q
                                  ),
                                  t =>
                                    Some(
                                      (
                                        t._1,
                                        t._2,
                                        t._3,
                                        t._4,
                                        t._5,
                                        t._6,
                                        t._7,
                                        t._8,
                                        t._9,
                                        t._10,
                                        t._11,
                                        t._12,
                                        t._13,
                                        t._14,
                                        t._15,
                                        t._16
                                      )
                                  )
                                )

                              def |@|[R](rr: F[R]): ProductBuilder[R] =
                                new ProductBuilder[R] {
                                  val r: F[R] = rr
                                }

                              sealed abstract class ProductBuilder[R] {
                                val r: F[R]

                                def apply[S](ff: (A,
                                                  B,
                                                  C,
                                                  D,
                                                  E,
                                                  G,
                                                  H,
                                                  I,
                                                  J,
                                                  K,
                                                  L,
                                                  M,
                                                  N,
                                                  O,
                                                  P,
                                                  Q,
                                                  R) => S,
                                             gg: S => Option[
                                               (A,
                                                B,
                                                C,
                                                D,
                                                E,
                                                G,
                                                H,
                                                I,
                                                J,
                                                K,
                                                L,
                                                M,
                                                N,
                                                O,
                                                P,
                                                Q,
                                                R)
                                             ]): F[S] =
                                  (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r)
                                    .xmapEither[S](
                                      {
                                        case (
                                            (
                                              (
                                                (
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          aa,
                                                                          bb
                                                                        ),
                                                                        cc
                                                                      ),
                                                                      dd
                                                                    ),
                                                                    ee
                                                                  ),
                                                                  gg
                                                                ),
                                                                hh
                                                              ),
                                                              ii
                                                            ),
                                                            jj
                                                          ),
                                                          kk
                                                        ),
                                                        ll
                                                      ),
                                                      mm
                                                    ),
                                                    nn
                                                  ),
                                                  oo
                                                ),
                                                pp
                                              ),
                                              qq
                                            ),
                                            rr
                                            ) =>
                                          Right(
                                            ff(
                                              aa,
                                              bb,
                                              cc,
                                              dd,
                                              ee,
                                              gg,
                                              hh,
                                              ii,
                                              jj,
                                              kk,
                                              ll,
                                              mm,
                                              nn,
                                              oo,
                                              pp,
                                              qq,
                                              rr
                                            )
                                          )
                                      },
                                      liftWrite(
                                        s =>
                                          gg(s).map {
                                            case (
                                                aa,
                                                bb,
                                                cc,
                                                dd,
                                                ee,
                                                gg,
                                                hh,
                                                ii,
                                                jj,
                                                kk,
                                                ll,
                                                mm,
                                                nn,
                                                oo,
                                                pp,
                                                qq,
                                                rr
                                                ) =>
                                              (
                                                (
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          (
                                                                            (
                                                                              aa,
                                                                              bb
                                                                            ),
                                                                            cc
                                                                          ),
                                                                          dd
                                                                        ),
                                                                        ee
                                                                      ),
                                                                      gg
                                                                    ),
                                                                    hh
                                                                  ),
                                                                  ii
                                                                ),
                                                                jj
                                                              ),
                                                              kk
                                                            ),
                                                            ll
                                                          ),
                                                          mm
                                                        ),
                                                        nn
                                                      ),
                                                      oo
                                                    ),
                                                    pp
                                                  ),
                                                  qq
                                                ),
                                                rr
                                              )
                                        }
                                      )
                                    )

                                def tupled =
                                  apply[
                                    (A,
                                     B,
                                     C,
                                     D,
                                     E,
                                     G,
                                     H,
                                     I,
                                     J,
                                     K,
                                     L,
                                     M,
                                     N,
                                     O,
                                     P,
                                     Q,
                                     R)
                                  ](
                                    (a: A,
                                     b: B,
                                     c: C,
                                     d: D,
                                     e: E,
                                     g: G,
                                     h: H,
                                     i: I,
                                     j: J,
                                     k: K,
                                     l: L,
                                     m: M,
                                     n: N,
                                     o: O,
                                     p: P,
                                     q: Q,
                                     r: R) =>
                                      (
                                        a,
                                        b,
                                        c,
                                        d,
                                        e,
                                        g,
                                        h,
                                        i,
                                        j,
                                        k,
                                        l,
                                        m,
                                        n,
                                        o,
                                        p,
                                        q,
                                        r
                                    ),
                                    t =>
                                      Some(
                                        (
                                          t._1,
                                          t._2,
                                          t._3,
                                          t._4,
                                          t._5,
                                          t._6,
                                          t._7,
                                          t._8,
                                          t._9,
                                          t._10,
                                          t._11,
                                          t._12,
                                          t._13,
                                          t._14,
                                          t._15,
                                          t._16,
                                          t._17
                                        )
                                    )
                                  )

                                def |@|[S](ss: F[S]): ProductBuilder[S] =
                                  new ProductBuilder[S] {
                                    val s: F[S] = ss
                                  }

                                sealed abstract class ProductBuilder[S] {
                                  val s: F[S]

                                  def apply[T](ff: (A,
                                                    B,
                                                    C,
                                                    D,
                                                    E,
                                                    G,
                                                    H,
                                                    I,
                                                    J,
                                                    K,
                                                    L,
                                                    M,
                                                    N,
                                                    O,
                                                    P,
                                                    Q,
                                                    R,
                                                    S) => T,
                                               gg: T => Option[
                                                 (A,
                                                  B,
                                                  C,
                                                  D,
                                                  E,
                                                  G,
                                                  H,
                                                  I,
                                                  J,
                                                  K,
                                                  L,
                                                  M,
                                                  N,
                                                  O,
                                                  P,
                                                  Q,
                                                  R,
                                                  S)
                                               ]): F[T] =
                                    (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s)
                                      .xmapEither[T](
                                        {
                                          case (
                                              (
                                                (
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          (
                                                                            (
                                                                              aa,
                                                                              bb
                                                                            ),
                                                                            cc
                                                                          ),
                                                                          dd
                                                                        ),
                                                                        ee
                                                                      ),
                                                                      gg
                                                                    ),
                                                                    hh
                                                                  ),
                                                                  ii
                                                                ),
                                                                jj
                                                              ),
                                                              kk
                                                            ),
                                                            ll
                                                          ),
                                                          mm
                                                        ),
                                                        nn
                                                      ),
                                                      oo
                                                    ),
                                                    pp
                                                  ),
                                                  qq
                                                ),
                                                rr
                                              ),
                                              ss
                                              ) =>
                                            Right(
                                              ff(
                                                aa,
                                                bb,
                                                cc,
                                                dd,
                                                ee,
                                                gg,
                                                hh,
                                                ii,
                                                jj,
                                                kk,
                                                ll,
                                                mm,
                                                nn,
                                                oo,
                                                pp,
                                                qq,
                                                rr,
                                                ss
                                              )
                                            )
                                        },
                                        liftWrite(
                                          t =>
                                            gg(t).map {
                                              case (
                                                  aa,
                                                  bb,
                                                  cc,
                                                  dd,
                                                  ee,
                                                  gg,
                                                  hh,
                                                  ii,
                                                  jj,
                                                  kk,
                                                  ll,
                                                  mm,
                                                  nn,
                                                  oo,
                                                  pp,
                                                  qq,
                                                  rr,
                                                  ss
                                                  ) =>
                                                (
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          (
                                                                            (
                                                                              (
                                                                                (
                                                                                  aa,
                                                                                  bb
                                                                                ),
                                                                                cc
                                                                              ),
                                                                              dd
                                                                            ),
                                                                            ee
                                                                          ),
                                                                          gg
                                                                        ),
                                                                        hh
                                                                      ),
                                                                      ii
                                                                    ),
                                                                    jj
                                                                  ),
                                                                  kk
                                                                ),
                                                                ll
                                                              ),
                                                              mm
                                                            ),
                                                            nn
                                                          ),
                                                          oo
                                                        ),
                                                        pp
                                                      ),
                                                      qq
                                                    ),
                                                    rr
                                                  ),
                                                  ss
                                                )
                                          }
                                        )
                                      )

                                  def tupled =
                                    apply[
                                      (A,
                                       B,
                                       C,
                                       D,
                                       E,
                                       G,
                                       H,
                                       I,
                                       J,
                                       K,
                                       L,
                                       M,
                                       N,
                                       O,
                                       P,
                                       Q,
                                       R,
                                       S)
                                    ](
                                      (a: A,
                                       b: B,
                                       c: C,
                                       d: D,
                                       e: E,
                                       g: G,
                                       h: H,
                                       i: I,
                                       j: J,
                                       k: K,
                                       l: L,
                                       m: M,
                                       n: N,
                                       o: O,
                                       p: P,
                                       q: Q,
                                       r: R,
                                       s: S) =>
                                        (
                                          a,
                                          b,
                                          c,
                                          d,
                                          e,
                                          g,
                                          h,
                                          i,
                                          j,
                                          k,
                                          l,
                                          m,
                                          n,
                                          o,
                                          p,
                                          q,
                                          r,
                                          s
                                      ),
                                      t =>
                                        Some(
                                          (
                                            t._1,
                                            t._2,
                                            t._3,
                                            t._4,
                                            t._5,
                                            t._6,
                                            t._7,
                                            t._8,
                                            t._9,
                                            t._10,
                                            t._11,
                                            t._12,
                                            t._13,
                                            t._14,
                                            t._15,
                                            t._16,
                                            t._17,
                                            t._18
                                          )
                                      )
                                    )

                                  def |@|[T](tt: F[T]): ProductBuilder[T] =
                                    new ProductBuilder[T] {
                                      val t: F[T] = tt
                                    }

                                  sealed abstract class ProductBuilder[T] {
                                    val t: F[T]

                                    def apply[U](ff: (A,
                                                      B,
                                                      C,
                                                      D,
                                                      E,
                                                      G,
                                                      H,
                                                      I,
                                                      J,
                                                      K,
                                                      L,
                                                      M,
                                                      N,
                                                      O,
                                                      P,
                                                      Q,
                                                      R,
                                                      S,
                                                      T) => U,
                                                 gg: U => Option[
                                                   (A,
                                                    B,
                                                    C,
                                                    D,
                                                    E,
                                                    G,
                                                    H,
                                                    I,
                                                    J,
                                                    K,
                                                    L,
                                                    M,
                                                    N,
                                                    O,
                                                    P,
                                                    Q,
                                                    R,
                                                    S,
                                                    T)
                                                 ]): F[U] =
                                      (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t)
                                        .xmapEither[U](
                                          {
                                            case (
                                                (
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          (
                                                                            (
                                                                              (
                                                                                (
                                                                                  aa,
                                                                                  bb
                                                                                ),
                                                                                cc
                                                                              ),
                                                                              dd
                                                                            ),
                                                                            ee
                                                                          ),
                                                                          gg
                                                                        ),
                                                                        hh
                                                                      ),
                                                                      ii
                                                                    ),
                                                                    jj
                                                                  ),
                                                                  kk
                                                                ),
                                                                ll
                                                              ),
                                                              mm
                                                            ),
                                                            nn
                                                          ),
                                                          oo
                                                        ),
                                                        pp
                                                      ),
                                                      qq
                                                    ),
                                                    rr
                                                  ),
                                                  ss
                                                ),
                                                tt
                                                ) =>
                                              Right(
                                                ff(
                                                  aa,
                                                  bb,
                                                  cc,
                                                  dd,
                                                  ee,
                                                  gg,
                                                  hh,
                                                  ii,
                                                  jj,
                                                  kk,
                                                  ll,
                                                  mm,
                                                  nn,
                                                  oo,
                                                  pp,
                                                  qq,
                                                  rr,
                                                  ss,
                                                  tt
                                                )
                                              )
                                          },
                                          liftWrite(
                                            u =>
                                              gg(u).map {
                                                case (
                                                    aa,
                                                    bb,
                                                    cc,
                                                    dd,
                                                    ee,
                                                    gg,
                                                    hh,
                                                    ii,
                                                    jj,
                                                    kk,
                                                    ll,
                                                    mm,
                                                    nn,
                                                    oo,
                                                    pp,
                                                    qq,
                                                    rr,
                                                    ss,
                                                    tt
                                                    ) =>
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          (
                                                                            (
                                                                              (
                                                                                (
                                                                                  (
                                                                                    (
                                                                                      aa,
                                                                                      bb
                                                                                    ),
                                                                                    cc
                                                                                  ),
                                                                                  dd
                                                                                ),
                                                                                ee
                                                                              ),
                                                                              gg
                                                                            ),
                                                                            hh
                                                                          ),
                                                                          ii
                                                                        ),
                                                                        jj
                                                                      ),
                                                                      kk
                                                                    ),
                                                                    ll
                                                                  ),
                                                                  mm
                                                                ),
                                                                nn
                                                              ),
                                                              oo
                                                            ),
                                                            pp
                                                          ),
                                                          qq
                                                        ),
                                                        rr
                                                      ),
                                                      ss
                                                    ),
                                                    tt
                                                  )
                                            }
                                          )
                                        )

                                    def tupled =
                                      apply[
                                        (A,
                                         B,
                                         C,
                                         D,
                                         E,
                                         G,
                                         H,
                                         I,
                                         J,
                                         K,
                                         L,
                                         M,
                                         N,
                                         O,
                                         P,
                                         Q,
                                         R,
                                         S,
                                         T)
                                      ](
                                        (a: A,
                                         b: B,
                                         c: C,
                                         d: D,
                                         e: E,
                                         g: G,
                                         h: H,
                                         i: I,
                                         j: J,
                                         k: K,
                                         l: L,
                                         m: M,
                                         n: N,
                                         o: O,
                                         p: P,
                                         q: Q,
                                         r: R,
                                         s: S,
                                         t: T) =>
                                          (
                                            a,
                                            b,
                                            c,
                                            d,
                                            e,
                                            g,
                                            h,
                                            i,
                                            j,
                                            k,
                                            l,
                                            m,
                                            n,
                                            o,
                                            p,
                                            q,
                                            r,
                                            s,
                                            t
                                        ),
                                        t =>
                                          Some(
                                            (
                                              t._1,
                                              t._2,
                                              t._3,
                                              t._4,
                                              t._5,
                                              t._6,
                                              t._7,
                                              t._8,
                                              t._9,
                                              t._10,
                                              t._11,
                                              t._12,
                                              t._13,
                                              t._14,
                                              t._15,
                                              t._16,
                                              t._17,
                                              t._18,
                                              t._19
                                            )
                                        )
                                      )

                                    def |@|[U](uu: F[U]): ProductBuilder[U] =
                                      new ProductBuilder[U] {
                                        val u: F[U] = uu
                                      }

                                    sealed abstract class ProductBuilder[U] {
                                      val u: F[U]

                                      def apply[V](ff: (A,
                                                        B,
                                                        C,
                                                        D,
                                                        E,
                                                        G,
                                                        H,
                                                        I,
                                                        J,
                                                        K,
                                                        L,
                                                        M,
                                                        N,
                                                        O,
                                                        P,
                                                        Q,
                                                        R,
                                                        S,
                                                        T,
                                                        U) => V,
                                                   gg: V => Option[
                                                     (A,
                                                      B,
                                                      C,
                                                      D,
                                                      E,
                                                      G,
                                                      H,
                                                      I,
                                                      J,
                                                      K,
                                                      L,
                                                      M,
                                                      N,
                                                      O,
                                                      P,
                                                      Q,
                                                      R,
                                                      S,
                                                      T,
                                                      U)
                                                   ]): F[V] =
                                        (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t zip u)
                                          .xmapEither[V](
                                            {
                                              case (
                                                  (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          (
                                                                            (
                                                                              (
                                                                                (
                                                                                  (
                                                                                    (
                                                                                      aa,
                                                                                      bb
                                                                                    ),
                                                                                    cc
                                                                                  ),
                                                                                  dd
                                                                                ),
                                                                                ee
                                                                              ),
                                                                              gg
                                                                            ),
                                                                            hh
                                                                          ),
                                                                          ii
                                                                        ),
                                                                        jj
                                                                      ),
                                                                      kk
                                                                    ),
                                                                    ll
                                                                  ),
                                                                  mm
                                                                ),
                                                                nn
                                                              ),
                                                              oo
                                                            ),
                                                            pp
                                                          ),
                                                          qq
                                                        ),
                                                        rr
                                                      ),
                                                      ss
                                                    ),
                                                    tt
                                                  ),
                                                  uu
                                                  ) =>
                                                Right(
                                                  ff(
                                                    aa,
                                                    bb,
                                                    cc,
                                                    dd,
                                                    ee,
                                                    gg,
                                                    hh,
                                                    ii,
                                                    jj,
                                                    kk,
                                                    ll,
                                                    mm,
                                                    nn,
                                                    oo,
                                                    pp,
                                                    qq,
                                                    rr,
                                                    ss,
                                                    tt,
                                                    uu
                                                  )
                                                )
                                            },
                                            liftWrite(
                                              v =>
                                                gg(v).map {
                                                  case (
                                                      aa,
                                                      bb,
                                                      cc,
                                                      dd,
                                                      ee,
                                                      gg,
                                                      hh,
                                                      ii,
                                                      jj,
                                                      kk,
                                                      ll,
                                                      mm,
                                                      nn,
                                                      oo,
                                                      pp,
                                                      qq,
                                                      rr,
                                                      ss,
                                                      tt,
                                                      uu
                                                      ) =>
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          (
                                                                            (
                                                                              (
                                                                                (
                                                                                  (
                                                                                    (
                                                                                      (
                                                                                        (
                                                                                          aa,
                                                                                          bb
                                                                                        ),
                                                                                        cc
                                                                                      ),
                                                                                      dd
                                                                                    ),
                                                                                    ee
                                                                                  ),
                                                                                  gg
                                                                                ),
                                                                                hh
                                                                              ),
                                                                              ii
                                                                            ),
                                                                            jj
                                                                          ),
                                                                          kk
                                                                        ),
                                                                        ll
                                                                      ),
                                                                      mm
                                                                    ),
                                                                    nn
                                                                  ),
                                                                  oo
                                                                ),
                                                                pp
                                                              ),
                                                              qq
                                                            ),
                                                            rr
                                                          ),
                                                          ss
                                                        ),
                                                        tt
                                                      ),
                                                      uu
                                                    )
                                              }
                                            )
                                          )

                                      def tupled =
                                        apply[
                                          (A,
                                           B,
                                           C,
                                           D,
                                           E,
                                           G,
                                           H,
                                           I,
                                           J,
                                           K,
                                           L,
                                           M,
                                           N,
                                           O,
                                           P,
                                           Q,
                                           R,
                                           S,
                                           T,
                                           U)
                                        ](
                                          (a: A,
                                           b: B,
                                           c: C,
                                           d: D,
                                           e: E,
                                           g: G,
                                           h: H,
                                           i: I,
                                           j: J,
                                           k: K,
                                           l: L,
                                           m: M,
                                           n: N,
                                           o: O,
                                           p: P,
                                           q: Q,
                                           r: R,
                                           s: S,
                                           t: T,
                                           u: U) =>
                                            (
                                              a,
                                              b,
                                              c,
                                              d,
                                              e,
                                              g,
                                              h,
                                              i,
                                              j,
                                              k,
                                              l,
                                              m,
                                              n,
                                              o,
                                              p,
                                              q,
                                              r,
                                              s,
                                              t,
                                              u
                                          ),
                                          t =>
                                            Some(
                                              (
                                                t._1,
                                                t._2,
                                                t._3,
                                                t._4,
                                                t._5,
                                                t._6,
                                                t._7,
                                                t._8,
                                                t._9,
                                                t._10,
                                                t._11,
                                                t._12,
                                                t._13,
                                                t._14,
                                                t._15,
                                                t._16,
                                                t._17,
                                                t._18,
                                                t._19,
                                                t._20
                                              )
                                          )
                                        )

                                      def |@|[V](vv: F[V]): ProductBuilder[V] =
                                        new ProductBuilder[V] {
                                          val v: F[V] = vv
                                        }

                                      sealed abstract class ProductBuilder[V] {
                                        val v: F[V]

                                        def apply[W](ff: (A,
                                                          B,
                                                          C,
                                                          D,
                                                          E,
                                                          G,
                                                          H,
                                                          I,
                                                          J,
                                                          K,
                                                          L,
                                                          M,
                                                          N,
                                                          O,
                                                          P,
                                                          Q,
                                                          R,
                                                          S,
                                                          T,
                                                          U,
                                                          V) => W,
                                                     gg: W => Option[
                                                       (A,
                                                        B,
                                                        C,
                                                        D,
                                                        E,
                                                        G,
                                                        H,
                                                        I,
                                                        J,
                                                        K,
                                                        L,
                                                        M,
                                                        N,
                                                        O,
                                                        P,
                                                        Q,
                                                        R,
                                                        S,
                                                        T,
                                                        U,
                                                        V)
                                                     ]): F[W] =
                                          (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t zip u zip v)
                                            .xmapEither[W](
                                              {
                                                case (
                                                    (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          (
                                                                            (
                                                                              (
                                                                                (
                                                                                  (
                                                                                    (
                                                                                      (
                                                                                        (
                                                                                          aa,
                                                                                          bb
                                                                                        ),
                                                                                        cc
                                                                                      ),
                                                                                      dd
                                                                                    ),
                                                                                    ee
                                                                                  ),
                                                                                  gg
                                                                                ),
                                                                                hh
                                                                              ),
                                                                              ii
                                                                            ),
                                                                            jj
                                                                          ),
                                                                          kk
                                                                        ),
                                                                        ll
                                                                      ),
                                                                      mm
                                                                    ),
                                                                    nn
                                                                  ),
                                                                  oo
                                                                ),
                                                                pp
                                                              ),
                                                              qq
                                                            ),
                                                            rr
                                                          ),
                                                          ss
                                                        ),
                                                        tt
                                                      ),
                                                      uu
                                                    ),
                                                    vv
                                                    ) =>
                                                  Right(
                                                    ff(
                                                      aa,
                                                      bb,
                                                      cc,
                                                      dd,
                                                      ee,
                                                      gg,
                                                      hh,
                                                      ii,
                                                      jj,
                                                      kk,
                                                      ll,
                                                      mm,
                                                      nn,
                                                      oo,
                                                      pp,
                                                      qq,
                                                      rr,
                                                      ss,
                                                      tt,
                                                      uu,
                                                      vv
                                                    )
                                                  )
                                              },
                                              liftWrite(
                                                w =>
                                                  gg(w).map {
                                                    case (
                                                        aa,
                                                        bb,
                                                        cc,
                                                        dd,
                                                        ee,
                                                        gg,
                                                        hh,
                                                        ii,
                                                        jj,
                                                        kk,
                                                        ll,
                                                        mm,
                                                        nn,
                                                        oo,
                                                        pp,
                                                        qq,
                                                        rr,
                                                        ss,
                                                        tt,
                                                        uu,
                                                        vv
                                                        ) =>
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          (
                                                                            (
                                                                              (
                                                                                (
                                                                                  (
                                                                                    (
                                                                                      (
                                                                                        (
                                                                                          (
                                                                                            (
                                                                                              aa,
                                                                                              bb
                                                                                            ),
                                                                                            cc
                                                                                          ),
                                                                                          dd
                                                                                        ),
                                                                                        ee
                                                                                      ),
                                                                                      gg
                                                                                    ),
                                                                                    hh
                                                                                  ),
                                                                                  ii
                                                                                ),
                                                                                jj
                                                                              ),
                                                                              kk
                                                                            ),
                                                                            ll
                                                                          ),
                                                                          mm
                                                                        ),
                                                                        nn
                                                                      ),
                                                                      oo
                                                                    ),
                                                                    pp
                                                                  ),
                                                                  qq
                                                                ),
                                                                rr
                                                              ),
                                                              ss
                                                            ),
                                                            tt
                                                          ),
                                                          uu
                                                        ),
                                                        vv
                                                      )
                                                }
                                              )
                                            )

                                        def tupled =
                                          apply[
                                            (A,
                                             B,
                                             C,
                                             D,
                                             E,
                                             G,
                                             H,
                                             I,
                                             J,
                                             K,
                                             L,
                                             M,
                                             N,
                                             O,
                                             P,
                                             Q,
                                             R,
                                             S,
                                             T,
                                             U,
                                             V)
                                          ](
                                            (a: A,
                                             b: B,
                                             c: C,
                                             d: D,
                                             e: E,
                                             g: G,
                                             h: H,
                                             i: I,
                                             j: J,
                                             k: K,
                                             l: L,
                                             m: M,
                                             n: N,
                                             o: O,
                                             p: P,
                                             q: Q,
                                             r: R,
                                             s: S,
                                             t: T,
                                             u: U,
                                             v: V) =>
                                              (
                                                a,
                                                b,
                                                c,
                                                d,
                                                e,
                                                g,
                                                h,
                                                i,
                                                j,
                                                k,
                                                l,
                                                m,
                                                n,
                                                o,
                                                p,
                                                q,
                                                r,
                                                s,
                                                t,
                                                u,
                                                v
                                            ),
                                            t =>
                                              Some(
                                                (
                                                  t._1,
                                                  t._2,
                                                  t._3,
                                                  t._4,
                                                  t._5,
                                                  t._6,
                                                  t._7,
                                                  t._8,
                                                  t._9,
                                                  t._10,
                                                  t._11,
                                                  t._12,
                                                  t._13,
                                                  t._14,
                                                  t._15,
                                                  t._16,
                                                  t._17,
                                                  t._18,
                                                  t._19,
                                                  t._20,
                                                  t._21
                                                )
                                            )
                                          )

                                        def |@|[W](
                                          ww: F[W]
                                        ): ProductBuilder[W] =
                                          new ProductBuilder[W] {
                                            val w: F[W] = ww
                                          }

                                        sealed abstract class ProductBuilder[W] {
                                          val w: F[W]

                                          def apply[X](ff: (A,
                                                            B,
                                                            C,
                                                            D,
                                                            E,
                                                            G,
                                                            H,
                                                            I,
                                                            J,
                                                            K,
                                                            L,
                                                            M,
                                                            N,
                                                            O,
                                                            P,
                                                            Q,
                                                            R,
                                                            S,
                                                            T,
                                                            U,
                                                            V,
                                                            W) => X,
                                                       gg: X => Option[
                                                         (A,
                                                          B,
                                                          C,
                                                          D,
                                                          E,
                                                          G,
                                                          H,
                                                          I,
                                                          J,
                                                          K,
                                                          L,
                                                          M,
                                                          N,
                                                          O,
                                                          P,
                                                          Q,
                                                          R,
                                                          S,
                                                          T,
                                                          U,
                                                          V,
                                                          W)
                                                       ]): F[X] =
                                            (a zip b zip c zip d zip e zip g zip h zip i zip j zip k zip l zip m zip n zip o zip p zip q zip r zip s zip t zip u zip v zip w)
                                              .xmapEither[X](
                                                {
                                                  case (
                                                      (
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          (
                                                                            (
                                                                              (
                                                                                (
                                                                                  (
                                                                                    (
                                                                                      (
                                                                                        (
                                                                                          (
                                                                                            (
                                                                                              aa,
                                                                                              bb
                                                                                            ),
                                                                                            cc
                                                                                          ),
                                                                                          dd
                                                                                        ),
                                                                                        ee
                                                                                      ),
                                                                                      gg
                                                                                    ),
                                                                                    hh
                                                                                  ),
                                                                                  ii
                                                                                ),
                                                                                jj
                                                                              ),
                                                                              kk
                                                                            ),
                                                                            ll
                                                                          ),
                                                                          mm
                                                                        ),
                                                                        nn
                                                                      ),
                                                                      oo
                                                                    ),
                                                                    pp
                                                                  ),
                                                                  qq
                                                                ),
                                                                rr
                                                              ),
                                                              ss
                                                            ),
                                                            tt
                                                          ),
                                                          uu
                                                        ),
                                                        vv
                                                      ),
                                                      ww
                                                      ) =>
                                                    Right(
                                                      ff(
                                                        aa,
                                                        bb,
                                                        cc,
                                                        dd,
                                                        ee,
                                                        gg,
                                                        hh,
                                                        ii,
                                                        jj,
                                                        kk,
                                                        ll,
                                                        mm,
                                                        nn,
                                                        oo,
                                                        pp,
                                                        qq,
                                                        rr,
                                                        ss,
                                                        tt,
                                                        uu,
                                                        vv,
                                                        ww
                                                      )
                                                    )
                                                },
                                                liftWrite(
                                                  x =>
                                                    gg(x).map {
                                                      case (
                                                          aa,
                                                          bb,
                                                          cc,
                                                          dd,
                                                          ee,
                                                          gg,
                                                          hh,
                                                          ii,
                                                          jj,
                                                          kk,
                                                          ll,
                                                          mm,
                                                          nn,
                                                          oo,
                                                          pp,
                                                          qq,
                                                          rr,
                                                          ss,
                                                          tt,
                                                          uu,
                                                          vv,
                                                          ww
                                                          ) =>
                                                        (
                                                          (
                                                            (
                                                              (
                                                                (
                                                                  (
                                                                    (
                                                                      (
                                                                        (
                                                                          (
                                                                            (
                                                                              (
                                                                                (
                                                                                  (
                                                                                    (
                                                                                      (
                                                                                        (
                                                                                          (
                                                                                            (
                                                                                              (
                                                                                                (
                                                                                                  aa,
                                                                                                  bb
                                                                                                ),
                                                                                                cc
                                                                                              ),
                                                                                              dd
                                                                                            ),
                                                                                            ee
                                                                                          ),
                                                                                          gg
                                                                                        ),
                                                                                        hh
                                                                                      ),
                                                                                      ii
                                                                                    ),
                                                                                    jj
                                                                                  ),
                                                                                  kk
                                                                                ),
                                                                                ll
                                                                              ),
                                                                              mm
                                                                            ),
                                                                            nn
                                                                          ),
                                                                          oo
                                                                        ),
                                                                        pp
                                                                      ),
                                                                      qq
                                                                    ),
                                                                    rr
                                                                  ),
                                                                  ss
                                                                ),
                                                                tt
                                                              ),
                                                              uu
                                                            ),
                                                            vv
                                                          ),
                                                          ww
                                                        )
                                                  }
                                                )
                                              )

                                          def tupled =
                                            apply[
                                              (A,
                                               B,
                                               C,
                                               D,
                                               E,
                                               G,
                                               H,
                                               I,
                                               J,
                                               K,
                                               L,
                                               M,
                                               N,
                                               O,
                                               P,
                                               Q,
                                               R,
                                               S,
                                               T,
                                               U,
                                               V,
                                               W)
                                            ](
                                              (a: A,
                                               b: B,
                                               c: C,
                                               d: D,
                                               e: E,
                                               g: G,
                                               h: H,
                                               i: I,
                                               j: J,
                                               k: K,
                                               l: L,
                                               m: M,
                                               n: N,
                                               o: O,
                                               p: P,
                                               q: Q,
                                               r: R,
                                               s: S,
                                               t: T,
                                               u: U,
                                               v: V,
                                               w: W) =>
                                                (
                                                  a,
                                                  b,
                                                  c,
                                                  d,
                                                  e,
                                                  g,
                                                  h,
                                                  i,
                                                  j,
                                                  k,
                                                  l,
                                                  m,
                                                  n,
                                                  o,
                                                  p,
                                                  q,
                                                  r,
                                                  s,
                                                  t,
                                                  u,
                                                  v,
                                                  w
                                              ),
                                              t =>
                                                Some(
                                                  (
                                                    t._1,
                                                    t._2,
                                                    t._3,
                                                    t._4,
                                                    t._5,
                                                    t._6,
                                                    t._7,
                                                    t._8,
                                                    t._9,
                                                    t._10,
                                                    t._11,
                                                    t._12,
                                                    t._13,
                                                    t._14,
                                                    t._15,
                                                    t._16,
                                                    t._17,
                                                    t._18,
                                                    t._19,
                                                    t._20,
                                                    t._21,
                                                    t._22
                                                  )
                                              )
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
  private def liftWrite[A, B](f: B => Option[A]): B => Either[String, A] =
    c =>
      f(c).fold[Either[String, A]](Left("Failed to write the value back."))(
        r => Right(r)
    )
}
