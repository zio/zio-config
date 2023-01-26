package zio.config

package object magnolia {
  def deriveConfig[A](implicit ev: Descriptor[A]) =
    ev.desc
}
