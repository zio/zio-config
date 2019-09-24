package zio.config

trait ConfigSyntax {
  implicit class ConfigSyntaxSyntaxOps[A](self: Config[A]) {
    final def <*>[B](f: Config[B]): ProductBuilder[A, B] =
      new ProductBuilder[A, B] {
        override val a: Config[A] = self
        override val b: Config[B] = f
      }
  }
}
