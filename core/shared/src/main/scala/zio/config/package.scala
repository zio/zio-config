package zio

import zio.config.syntax.{ConfigSyntax, KeyComponent}

package object config
    extends KeyConversionFunctions
    with ConfigSyntax
    with ImplicitTupleConversion
    with ConfigDocsModule {

  implicit class Interpolator(private val sc: StringContext) extends AnyVal {
    def path(str: String*): Chunk[KeyComponent] =
      Chunk.fromIterable(sc.s(str: _*).split('.')).flatMap(str => KeyComponent.getPath(str))
  }
}
