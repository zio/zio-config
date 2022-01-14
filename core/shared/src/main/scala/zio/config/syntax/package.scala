package zio.config

import zio._

package object syntax {
  final implicit class ZIOConfigNarrowOps[R, E, A](
    private val self: ZLayer[R, E, A]
  ) extends AnyVal {

    /**
     * In bigger apps you can have a lot of components and, consequently - a lot of configuration fields.
     * It's not ideal to pass around the whole configuration object as a dependency for all of those components: this way you break the separation of concerns principle.
     * Component should be aware only about dependencies it cares about and uses somehow.
     *
     * So to avoid that and do it in an ergonomic way, there's a narrow syntax extension for ZLayer, available under import zio.config.syntax._.
     * In the below example, we use both zio-config-magnolia and zio-config-typesafe module inorder to have automatic derivation as well as read/write
     * HOCON structure.
     *
     * {{{
     *
     *  import zio._
     *  import zio.config._, typesafe._, syntax._, magnolia.DeriveConfigDescriptor
     *
     *  trait Endpoint
     *  trait Repository
     *
     *  case class AppConfig(api: ApiConfig, db: DbConfig)
     *  case class DbConfig (url: String,    driver: String)
     *  case class ApiConfig(host: String,   port: Int)
     *
     *  val configDescription = DeriveConfigDescriptor.descriptor[AppConfig]
     *
     *  // components have only required dependencies
     *  val endpoint: ZLayer[Has[ApiConfig], Nothing, Has[Endpoint]]    = ZLayer.fromService(_ => new Endpoint {})
     *  val repository: ZLayer[Has[DbConfig], Nothing, Has[Repository]] = ZLayer.fromService(_ => new Repository {})
     *
     *  val cfg = TypesafeConfig.fromResourcePath(configDescription)
     *
     *  cfg.narrow(_.api) >>> endpoint // narrowing down to a proper config subtree
     *  cfg.narrow(_.db) >>> repository
     *
     * }}}
     */
    def narrow[B: Tag](
      f: A => B
    )(implicit ta: Tag[A], ev: IsNotIntersection[A], ev2: IsNotIntersection[B]): ZLayer[R, E, B] =
      self.map(a => ZEnvironment[B](f(a.get[A])))
  }
}
