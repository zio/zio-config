package zio

package object config
    extends KeyConversionFunctions
    with syntax.ConfigSyntax
    with ImplicitTupleConversion
    with ConfigDocsModule {

  implicit class Interpolator(private val sc: StringContext) extends AnyVal {
    def path(str: String*): Chunk[String] =
      Chunk.fromIterable(sc.s(str: _*).split('.'))
  }
}
