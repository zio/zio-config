package zio

package object config
    extends KeyConversionFunctions
    with syntax.ConfigSyntax
    with ImplicitTupleConversion
    with ConfigDocsModule {

  /**
   * Adds the `path` string interpolator, which splits a dotted key into its parts.
   */
  implicit class Interpolator(private val sc: StringContext) extends AnyVal {

    /**
     * Builds the string as the `s` interpolator does, then splits it at each `.` into a `Chunk[String]`. Interpolated
     * values are split too: with `val parent = "app.db"`, `path"\$parent.port"` gives `Chunk(app, db, port)`. An empty
     * part between two dots is kept, and trailing empty parts are dropped, as `String#split` does.
     *
     * {{{
     * import zio._
     * import zio.config._
     *
     * val key: Chunk[String] = path"server.port"
     * // Chunk(server,port)
     * }}}
     */
    def path(str: String*): Chunk[String] =
      Chunk.fromIterable(sc.s(str: _*).split('.'))
  }
}
