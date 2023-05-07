package zio

import zio.config.syntax.ConfigSyntax

package object config
    extends KeyConversionFunctions
    with ConfigSyntax
    with ImplicitTupleConversion
    with ConfigDocsModule {

  implicit class Interpolator(private val sc: StringContext) extends AnyVal {
    def path(str: String*): Chunk[String] =
      Chunk.fromIterable(sc.s(str: _*).split('.'))
  }

  implicit final class ReloadableSyntax[Any, E, ROut](private val layer: ZLayer[Any, E, ROut]) extends AnyVal {
    def reloadable(implicit
      tag: Tag[ROut],
      serviceProxy: ServiceProxy[ROut]
    ): ZLayer[ServiceReloader, ServiceReloader.Error, ROut] =
      ZLayer.fromZIO(ServiceReloader.register(self))
  }
}
