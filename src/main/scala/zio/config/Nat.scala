package zio.config

trait ~>[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}
